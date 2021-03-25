## function that takes in data, a fit model, and prepped recipe object, general specs, makes predictions, and outputs eval stats
## data: is expected to have a response variable and weight
## mod_obj: a specified fit model
## ped_rec: a prepped recipe object
## spec_gen: a list containing general specifications that are common across all mod_fitters, and spec_recs
eval_mod <- function(data, mod_obj, ped_rec, spec_gen){
  source('tidy_models/src/tidy_pred.R')
  
  data$.pred <- tidy_pred(data, mod_obj, ped_rec, spec_gen)
  
  dev <- deviance_tidy(spec_gen$family)(data %$% get(spec_gen$response) , data$.pred, data %$% get(spec_gen$weight))
  gini <- gini_tidy(data %$% get(spec_gen$response) , data$.pred, data %$% get(spec_gen$weight))
  
  tibble('.metric' = 'dev', '.estimator' = 'standard', '.estimate' = dev) %>%
    bind_rows(tibble('.metric' = 'gini', '.estimator' = 'standard', '.estimate' = gini))
}