#' Computes confidence intervals using the T distribution
#'
#'
#' @param .mean  the sample mean for which the CI is being estimated
#' @param .sd The sample standard deviation
#' @param .n The sample size
#' @param .t_crit the critical T value
#' @param .return One of \code{c("both","low","hi")}. Defaults to \code{both}, returning a list with the lower and upper limits
#' @return a list with the lower and upper confidence limits, or
#' @family point estimate helpers
#' @rdname tdist_ci
#' @export
#'
#'
tdist_ci <-
  function(.mean, .sd, .n, .t_crit,.return=c("both", "low","hi")){
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
