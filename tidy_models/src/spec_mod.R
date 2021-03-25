## function takes a model object, and specs list and returns a function that can be called later to fit a model.
## Said later function only needs data and has the specs saved and ready to be used whenever re-called.
## Inner function defines the specific model construct. This function can be added to in order to support 
## any modelling algorithm, the rest of the code makes no mention to specific models, provided
## that all the modelling paramaters can be passed in from a specs list. 
## For ex) 'spec_mod(mod = xgboost, specs = specs)'.
## Anything in specs list can be tuned over using nested CV.
## code is set up in this way because in order to properly do cross validation the same model construct
## needs to be fit repeatedly, hence the need to seperate specification from model fitting
## mod: pre-existing model algorithm for example glm, xgboost (future)
## specs: list containing specifications to later fit the model however the construct is defined below
spec_mod <- function(mod, specs){
  mod_fitter <- function(data){
    if(identical(mod, glm)){
      response <- specs$response
      mod_formula <- gen_formula(specs$response, specs$xvars, specs$xvars_pow, specs$interactions, specs$offsets)
      family <- paste0(specs$family, '()')
      eval(
        parse(text = 
                paste0(
                  'mod(mod_formula, data = data, weight = ', specs$weight, ', family = ', specs$family, ')'
                )
        )
      )
    }else if(identical(mod, lgb.train)){
      #custom.folds <- createFolds(data, fold.var)
      #, folds = custom.folds #lgb.cv()
      
      target_col <- data %$% get(specs$response)
      weight_col <- data %$% get(specs$weight)
      data <- data %>% select(-all_of(specs$response), -all_of(specs$weight)) %>%
        as.matrix()
      
      train_lgb = lgb.Dataset(data = data, label = target_col, categorical_feature = specs$categorical_features
                              , weight = weight_col)
      
      lgbfit <- lgb.cv(params = specs$params, data = train_lgb, verbose = 0,  num_threads = 4,
                       nrounds = 200, early_stopping_rounds = 50, eval_freq=100)
      
      # LightGBM training
      lgb.train(params= specs$params, train_lgb, nrounds = lgbfit$best_iter, verbose = 0, num_threads = 4)
      
    }
    
  }
  
  attr(mod_fitter, 'specs') <- specs
  return(mod_fitter)
}