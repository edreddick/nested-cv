## function to takes in data, user defined list of specified but unfit models, user defined list of specified but unprepped
## recipes, and list of general model specifications. Splits between train and test, tunes across list of mod_fitters, spec_recs.
## fits final model based upon best tuning results using grid search.
## evaluates exact same tuning protocol using nested CV. evaluates final model on testing data.
## returns list containing fit model, prepped recipe, nested CV result, testing data result
## note: if mod_fitters is length 1 then tuning is not required and process carries out regular CV.
## mod_fitters: a list containing specified model objects to tune over
## spec_recs: a list containing multiple un-prepped recipe specs
## spec_gen: a list containing general specifications that are common across all mod_fitters, and spec_recs
build_mod <- function(data, mod_fitters, spec_recs, spec_gen){
  source('tidy_models/src/deviance_tidy.R')
  source('tidy_models/src/gini_tidy.R')
  source('tidy_models/src/gen_formula.R')
  source('tidy_models/src/eval_mod.R')
  source('tidy_models/src/eval_mod_cv.R')
  source('tidy_models/src/inner_cv.R')
  source('tidy_models/src/choose_spec.R')
  source('tidy_models/src/outer_cv.R')
  
  set.seed(1234)
  ## filter based on coverage of interest of interest
  data <- data %>% 
    filter(covs == spec_gen$cov)
  
  ## filter based on weight
  data <- data %>% 
    filter(get(spec_gen$weight) > 0)
  
  ## select only needed variables
  rm_vars <- names(data)[!names(data) %in% c(spec_gen$response, spec_gen$weight, spec_gen$xvars, spec_gen$offsets)]
  data <- data %>% 
    select(-all_of(rm_vars))
  gc()
  
  ## Step 0: EDA (Exploratory Data Analaysis) (skip for now)
  
  ## Step 1: Initial Data Split
  x_split <- rsample::initial_split(
    data, 
    prop = 0.8, 
    strata = all_of(spec_gen$response)
  )
  
  ## Step 2: Preprocessing Specification - user defined list functions that contain recipe steps (spec_recs)
  ## This creates the appropriate string:
  rec_formula <- as.formula(paste0(spec_gen$response, ' ~ .'))
  
  ## Step 3: Model Specification - user defined list functions that fit models (mod_fitters)
  
  ## Step 4: Tune across mod_fitters (list of functions containing specified but unfit models) 
  ## pick best set of paramaters using CV grid search and fit final trained model
  best_spec <- choose_spec(training(x_split), mod_fitters, spec_recs, spec_gen)
  
  mod_fitter <- best_spec$mod_fitter
  
  ped_rec <- best_spec$spec_rec(rec_formula, spec_gen)(training(x_split)) %>%
    prep()
  
  mod_obj <- mod_fitter(data = juice(ped_rec))
  
  ## Step 5: Cross Validation, Evaluate Performance on Training Data
  ## use nested CV to evaluate performance of model as well as automatic model configuration procedure
  results_outer_cv <- outer_cv(training(x_split), mod_fitters, spec_recs, spec_gen)
  
  ## Step 6: Evaluate Performance on Test Data
  results_testing_eval_mod <- eval_mod(testing(x_split), mod_obj, ped_rec, spec_gen)
  
  out <- list(ped_rec = ped_rec, 
              mod_obj = mod_obj,
              results_outer_cv = results_outer_cv,
              results_testing_eval_mod = results_testing_eval_mod)
  
  return(out)
}