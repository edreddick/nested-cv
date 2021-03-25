deviance_tidy <- function(family){
  
  gamma.deviance <- function(obs.total, pred.total, count){
    #Experimental - Proof but unchecked
    
    non.zero.count <- (count != 0)
    
    obs.avg <- obs.total[non.zero.count]/count[non.zero.count]
    pred.avg <- pred.total[non.zero.count]/count[non.zero.count]
    
    unweighted.vec <- -log(pred.avg) - obs.avg/pred.avg
    weighted.vec <- count[non.zero.count]*unweighted.vec
    
    -sum(weighted.vec)/sum(count)
  }
  
  poisson.deviance <- function(obs.count, pred.count, exposure){
    
    non.zero.exposure <- (exposure != 0)
    obs.count <- obs.count[non.zero.exposure]
    pred.count <- pred.count[non.zero.exposure]
    exposure <- exposure[non.zero.exposure] 
    
    obs.rate <- obs.count/exposure
    pred.rate <- pred.count/exposure
    unweighted.vec <- obs.rate*log(pred.rate) - pred.rate
    weighted.vec <- exposure*unweighted.vec
    -sum(weighted.vec)/sum(exposure)
  }
  
  binomial.deviance <-  function(obs, pred, weights = rep(1,length(obs)), family="binomial", calc.mean = TRUE){
    if (length(obs) != length(pred)) {   stop("observations and predictions must be of equal length") }
    y_i <- obs
    u_i <- pred
    family = tolower(family)
    if (family == "binomial" | family == "bernoulli") {
      deviance.contribs <- (y_i * log(u_i)) + ((1-y_i) * log(1 - u_i))
      deviance <- -2 * sum(deviance.contribs * weights)
    } else if (family == "poisson") {
      deviance.contribs <- ifelse(y_i == 0, 0, (y_i * log(y_i/u_i))) - (y_i - u_i)
      deviance <- 2 * sum(deviance.contribs * weights)
    } else if (family == "laplace") {
      deviance <- sum(abs(y_i - u_i))
    } else if (family == "gaussian") {
      deviance <- sum((y_i - u_i) * (y_i - u_i))
    } else {
      stop('unknown family, should be one of: "binomial", "bernoulli", "poisson", "laplace", "gaussian"')
    }
    if (calc.mean) deviance <- deviance/length(obs)
    return(deviance)
  }
  
  ## return proper function based on family
  if(family == 'poisson'){
    return(poisson.deviance)
  }else if(family == 'gamma'){
    return(gamma.deviance)
  }else if(family == 'binomial'){
    return(binomial.deviance)
  }
}