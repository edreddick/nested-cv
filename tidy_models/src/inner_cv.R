## function that takes in data, mod_fitter, spec_rec splits data into folds proportional to response
## calls eval_mod_cv for each of the folds and then aggregates across folds for cross validation results
## data: expected to have response, weight, predictor vars etc
## mod_fitter: a specified unfit model
## spec_rec: a specified un-prepped function that calls a recipe
## spec_gen: a list containing general specifications that are common across all mod_fitters, and spec_recs
inner_cv <- function(data, mod_fitter, spec_rec, spec_gen){
  folds <- vfold_cv(data, v = 5, strata = all_of(spec_gen$response))
  map_df(folds$splits, ~ eval_mod_cv(.x, mod_fitter, spec_rec, spec_gen)) %>%
    group_by(.metric) %>%
    summarise(.estimate = mean(.estimate))
}