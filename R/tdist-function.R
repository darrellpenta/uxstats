#' Find a T-distribution probability
#'
#' @description
#' Calculates the one- or two-tailed probability density for Student's T Distribution
#'
#' Similar to Excel's \code{TDIST(abs(x),df,tails)} function.

#' @param .p  The numeric probability associated with the two-tailed T-distribution. Must be greater than 0 and less than 1.
#' @param .df The number of degrees of freedom.Must be greater than 0 and less than 1.
#' @param .tail One of \code{1} or \code{2} (Default) indicating whether to return the one-tailed or two-tailed value.
#' @return Returns the probability for the Student T-distribution when .x is given as the value of T.
#' @importFrom stats pt
#' @family t-distribution functions (~ Excel)
#' @rdname tdist
#' @export
#'
tdist <-
  function(.p, .df, .tail=c(2,1)){
    .tail <- match.arg(.tail)
  if(!is.numeric(.p)){
    stop(".p must be a numeric")
  }
  if(!is.numeric(.df)){
    stop(".df must be a numeric")
  }
  if(.p == 0){
    stop(".p cannot be equal to zero")
  }
 stats::pt(-abs(.p), .df) * .tail
  }
