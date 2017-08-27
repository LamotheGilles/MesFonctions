#' PRESS Function
#'
#' This function computes Allen's PRESS for an lm object
#' @param model An lm object. (Requires input)
#' @keywords PRESS
#' @export
#' @examples
#' x<-runif(1:20); y<-2+3*x+rnorm(20,0,5); fit<-lm(y~x)
#' PRESS(fit)
#' x<-runif(1:20); y<-2+3*x+rnorm(20,0,5); fit2<-lm(y~x)
#' PRESS(fit,fit2)


PRESS <- function(model,...)
{
   
if (length(list(...))) { 
     model <- list(model, ...)
     PRESS.lonely<-function(lm.model)
           {
              rPrevu<-stats::residuals(lm.model)/(1 - stats::lm.influence(lm.model)$hat)
              return(sum(rPrevu^2))

           }
     return(sapply(model, PRESS.lonely))
     }
else { rPrevu<-stats::residuals(model)/(1 - stats::lm.influence(model)$hat)
      return(sum(rPrevu^2))
     }

}


