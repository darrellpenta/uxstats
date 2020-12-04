#' Computes confidence intervals on the difference of means for within-subjects tests
#'
#'
#' @param .mean  the sample mean for which the CI is being estimated
#' @param .sd The sample standard deviation
#' @param .n The sample size
#' @param .alpha the critical T value
#' @param .return One of \code{c("both","low","hi")}. Defaults to \code{both}, returning a list with the lower and upper limits
#' @return a list with the lower and upper confidence limits, or either limit if \code{.return} is specified.
#' @family point estimate helpers
#' @rdname within_diff_ci
#' @export
#'
#'
within_diff_ci <-
  function(.mean, .sd, .n, .alpha,.return=c("both", "low","hi")){
    .t_crit<- abs(qt((.alpha/2), (.n-1)))
    .ci_low <- .mean-(.t_crit*(.sd/sqrt(.n)))
    .ci_hi <- .mean+(.t_crit*(.sd/sqrt(.n)))
    .return<-match.arg(.return)
    if (.return == "low") {
      return(.ci_low)
    } else if (.return == "hi"){
      return(.ci_hi)
    } else {
      return(list(.ci_low, .ci_hi))
    }
  }
