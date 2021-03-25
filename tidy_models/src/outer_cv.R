## function carries out nested cross validation in order to unbiasedly evaluate automatic model configuration procedure,
## pre-processing, and all other constraints. Results from this can be used directly to make modelling decisions without
## fear of overfitting to the test data.
## data: expected to have response, weight, predictor vars etc
## mod_fitters: a list containing specified model objects to tune over
## spec_recs: a list containing multiple un-prepped recipe specs
## spec_gen: a list containing general specifications that are common across all mod_fitters, and spec_recs
## inner layers of CV calculate best model spec for each fold and pass the model specs up to the outer layer of CV
## to be used to evaluate
## For more on nested CV: https://machinelearningmastery.com/nested-cross-validation-for-machine-learning-with-python/
## https://weina.me/nested-cross-validation/
outer_cv <- function(data, mod_fitters, spec_recs, spec_gen){
  
  folds <- vfold_cv(data, v = 5, strata = all_of(spec_gen$response))
  
  best_spec <- map(folds$splits, ~ choose_spec(training(.x), mod_fitters, spec_recs, spec_gen))
  
  mod_fitters_inner <- map(best_spec, ~ pluck(.x, 'mod_fitter'))
  spec_rec_inner <- map(best_spec, ~ pluck(.x, 'spec_rec'))
  
  pmap_df(list(folds$splits, mod_fitters_inner, spec_rec_inner), eval_mod_cv, spec_gen = spec_gen) %>%
    group_by(.metric) %>%
    summarise(.estimate = mean(.estimate))
}