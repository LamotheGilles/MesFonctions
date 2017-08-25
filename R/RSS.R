#' RSS Function
#'
#' This function computes the residual sum of squares for an lm object
#' @param model An lm object. (Requires input)
#' @keywords RSS
#' @export
#' @examples
#' x<-runif(1:20); y<-2+3*x+rnorm(20,0,5); fit<-lm(y~x)
#' RSS(fit)
RSS <- function(model)
{
sum(stats::residuals(model)^2)
}

