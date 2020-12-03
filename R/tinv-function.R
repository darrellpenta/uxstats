#' Calculate the inverse of the two-tailed Student's T Distribution
#'
#' Equivalent to Excel's \code{TINV(x,df)} function.
#'
#' @param .a  The alpha level, or the numeric probability associated with the T-distribution. Must be greater than 0 and less than 1.
#' @param .df The number of degrees of freedom. Must be greater than 0 and less than 1.
#' @param .tail One of \code{1} or \code{2} (Default) indicating whether to return the one-tailed or two-tailed value.
#' @param ... (Optional) Named arguments to be passed to \code{stats::qt()}.
#' @return Returns the (absolute) T-critical value.
#' @importFrom stats qt
#' @family t-distribution functions (~ Excel)
#' @rdname tinv
#' @export
#'
tinv <-
  function(.a, .df, .tail = 2, ...){
  if(!is.numeric(.a)){
    stop(".a must be a numeric")
  }
  if(!is.numeric(.df)){
    stop(".df must be a numeric")
  }
  if(.a == 0){
    stop(".a cannot be equal to zero")
  }

    if(.tail==1){
    abs(stats::qt(.a, df=.df, ...))
  } else {
   abs(stats::qt(.a/2, df=.df, ...))
  }
}
