#' Computes confidence intervals on the difference of means for between-subjects tests
#'
#'
#'
#' @param .x  the mean from sample 1.
#' @param .sd  the standard deviation from sample 1.
#' @param .n  the size of sample 1.
#' @param .x2  the mean from sample 2.
#' @param .sd2  the standard deviation from sample 2.
#' @param .n2  the size of sample 2.
#' @param .t the critical T value
#' @param .return One of \code{c("both","low","hi")}. Defaults to \code{both}, returning a list with the lower and upper limits
#' @return a list with the lower and upper confidence limits, or either limit if \code{.return} is specified.
#' @family point estimate helpers
#' @rdname between_diff_ci
#' @export
#'
#'
between_diff_ci <-
  function(.x, .sd, .n, .x2, .sd2, .n2,.t,.return=c("both", "low","hi")){
    .v<-sqrt((.sd^2/.n) + (.sd2^2/.n2))
    .ci_low <- (.x-.x2)-(.t*.v)
    .ci_hi <- (.x-.x2)+(.t*.v)
    .return<-match.arg(.return)
    if (.return == "low") {
      return(.ci_low)
    } else if (.return == "hi"){
      return(.ci_hi)
    } else {
      return(list(.ci_low, .ci_hi))
    }
  }
