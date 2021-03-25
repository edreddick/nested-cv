gini_tidy <- function(actual,predicted,weights){
  pred.per.unit <- predicted/weights
  order.of.predictions <- order(-pred.per.unit)
  
  ordered.predictions <- c(0,predicted[order.of.predictions])
  ordered.actual <- c(0,actual[order.of.predictions])
  ordered.weights <- c(0,weights[order.of.predictions])
  
  cumulative.weight <- cumsum(ordered.weights)/sum(ordered.weights)
  cumulative.actual <- cumsum(ordered.actual)/sum(ordered.actual)
  
  trapezoid.widths <- diff(cumulative.weight,lag=1)
  trapezoid.height.1 <- cumulative.actual[1:(length(cumulative.actual)-1)]
  trapezoid.height.2 <- cumulative.actual[2:length(cumulative.actual)]
  
  trapezoid.area <- trapezoid.widths*(trapezoid.height.1 + trapezoid.height.2)/2
  
  AUC <- sum(trapezoid.area)
  gini <- 2*AUC - 1
  gini
}
