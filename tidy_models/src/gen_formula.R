## function use to generate model formula
gen_formula <- function(response, xvars, xvars_pow, interactions, offsets){
  
  ## function used to manlipulate formula string, raise variables to a higher polynomial power
  gen_pows <- function(x, n){
    gen_pow <- function(x, y){
      paste0('I(', x, '^', y, ')')
    }
    if(n >= 2){
      map_chr(2:n, ~ gen_pow(x, .x))
    }
  }
  
  ## function used to manlipulate formula string to generate offset terms
  gen_offset <- function(x, y){
    if(is_character(x)){
      if(y == 'poisson'){
        paste0('offset(log(', x, '))')
      }else if(y == 'gamma'){
        paste0('offset(-1/', x, ')')
      }
    }
  }
  
  xterms_pow <- map2(xvars, xvars_pow, ~ gen_pows(.x, .y)) %>%
    unlist()
  
  offsets_formula <- gen_offset(offsets, family)
  
  xterms <- c(xvars, xterms_pow, interactions, offsets_formula)
  
  return(as.formula(paste(response, c(paste(xterms, collapse=' + ')), sep=' ~ ')))
}
