## function that takes in data, a specified model, prep recipe based on training data data, 
## fits specifed model, calls eval_mod to eval fit on testing data
## data: is expected to be a resample object with a training and testing split
## mod_fitter: a specified unfit model
## spec_rec: a specified un-prepped function that calls a recipe
## spec_gen: a list containing general specifications that are common across all mod_fitters, and spec_recs
eval_mod_cv <- function(data, mod_fitter, spec_rec, spec_gen){
  
  rec_formula <- as.formula(paste0(spec_gen$response, ' ~ .'))
  ## recipe uses same global spec_rec, but steps are calculated based on input folds of data instead of training data
  ## this is to avoid leakage that would occur from using the full training data to calc pre-processing during CV
  ped_rec <- spec_rec(rec_formula, spec_gen)(training(data)) %>%
    prep()
  
  mod_obj <- mod_fitter(data = juice(ped_rec))
  
  eval_mod(testing(data), mod_obj, ped_rec, spec_gen)
}
