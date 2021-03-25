## function that takes in data, a fit model, and prepped recipe object, general specs, makes predictions, and 
## makes associated predictions, currently supports glm and lgbm.
## data: is expected to have a response variable and weight
## mod_obj: a specified fit model
## ped_rec: a prepped recipe object
## spec_gen: a list containing general specifications that are common across all mod_fitters, and spec_recs
tidy_pred <- function(data, mod_obj, ped_rec, spec_gen){
  if(class(mod_obj)[1] == 'glm'){
    data$.pred <- predict(mod_obj, newdata = bake(ped_rec, new_data = data), type = 'res')
  }else if(class(mod_obj)[1] == 'lgb.Booster'){
    lgb_pred <- bake(ped_rec, new_data = data) %>%
      select(-all_of(spec_gen$response), -all_of(spec_gen$weight)) %>% 
      as.matrix()
    data$.pred <- predict(mod_obj, data = lgb_pred)
  }
  data$.pred
}