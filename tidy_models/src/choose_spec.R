## function that takes in data, mod_fitters, spec_recs, uses CV grid search and outputs best model specification function object
## if mod_fitters object is only length 1 then the function just chooses the mod_fitter in the list
## data: expected to have response, weight, predictor vars etc
## mod_fitters: a list containing specified model objects to tune over
## spec_recs: a list containing un-prepped recipe specs
## spec_gen: a list containing general specifications that are common across all mod_fitters, and spec_recs
choose_spec <- function(data, mod_fitters, spec_recs, spec_gen){
  
  if(length(mod_fitters) == 1){
    list(mod_fitter = mod_fitters[[1]], spec_rec = spec_recs[[1]])
  }else{
    best_row <- map2_df(mod_fitters, spec_recs, ~ inner_cv(data, .x, .y, spec_gen), .id = 'grid') %>%
      filter(.metric == "dev") %>%
      slice_min(.estimate) %$% 
      grid %>%
      as.numeric
    
    list(mod_fitter = mod_fitters[[best_row]], spec_rec = spec_recs[[best_row]])
  }
}
