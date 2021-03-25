library(magrittr)
library(purrr)
library(tidyverse)
library(tidymodels)
library(lightgbm)


source('tidy_models/src/spec_mod.R')
source('tidy_models/src/build_mod.R')

############################### Create fake small dataset ###############################
n <- 10000
beta0 <- 1
beta1 <- 0.2
beta2 <- .25

num_a <- runif(n = n, min = 0, max=1.5)
char_a <- sample(c('a', 'b'), size = n, replace = TRUE)
covs <- sample(c('A', 'B', 'C'), size = n, replace = TRUE)
exposure <- runif(n=n, min=.01, max=1)
mu <- exp(beta0 + beta1 * num_a + beta2 * (as.numeric(as.factor(char_a))-1))/exposure
count <- rpois(n=n, lambda = mu)
  
df <- tibble(count, mu, exposure, covs, num_a, char_a)
rm(n, beta0, beta1, beta2, num_a, char_a, exposure, count, covs)

############################### start user defined elements - to be stripped out and passed in using json ##########
spec_gen <- list(covs = 'A', response = 'count', weight = 'exposure', family = 'poisson', 
                 xvars  = c('num_a', 'char_a'), 
                 offsets =  c())

specs1 <- list(response = spec_gen$response, 
               params = list(objective = spec_gen$family, learning_rate = 0.1, num_leaves = 15
                             , max_depth = 5, subsample = 0.7, colsample_bytree = 0.8
                             , min_data_in_leaf = 20, is_unbalance = TRUE) 
               , weight = spec_gen$weight, categorical_features = 'char_a')

specs2 <- specs1
specs2$max_depth <- 4

## function that returns a function that contains pre-processing specifications
## inner function only needs data and all pre-processing is defined in the inner function.
## code is set up in this way because in order to properly do cross validation the same pre-processing construct
## needs to be preped repeatedly on different folds, otherwise modelling process suffers from leakage.
## For example, if part of the pre-processing is to impute NA values with column means, this needs to occur within
## a CV fold not before a CV fold.
spec_rec_lgbm <- function(rec_formula, spec_gen){
  fit_rec <- function(data){
    recipe(rec_formula, data = data) %>%
      update_role(all_of(spec_gen$weight), new_role = "case weight") %>%
      step_zv(all_numeric()) %>%
      step_nzv(all_nominal()) %>%
      step_other(all_nominal(), threshold = 0.01) %>%
      step_factor2string(all_nominal(), -all_outcomes()) %>%
      step_string2factor(all_nominal(), -all_outcomes(), ordered = TRUE) %>%
      step_ordinalscore(all_nominal())
  }
  return(fit_rec)
}

spec_recs <- list(spec_rec_lgbm, spec_rec_lgbm); rm(spec_rec_lgbm)
mod_fitter1 <- spec_mod(mod = lgb.train, specs = specs1)
mod_fitter2 <- spec_mod(mod = lgb.train, specs = specs2)
mod_fitters <- list(mod_fitter1, mod_fitter2); rm(mod_fitter1, mod_fitter2, specs1, specs2)
############################### end user defined elements###############################

## return prepped recipe, trained model, nested CV evaluation, holdout evaluation
ptm <- proc.time()
final_mod <- build_mod(df, mod_fitters, spec_recs, spec_gen)
final_time <- proc.time() - ptm
