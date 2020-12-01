#' Computes success rate using Wilson estimate
#'
#'
#' @param .success  the total number of successes
#' @param .trials the total number of trials
#' @param .Z The Z score corresponding to the desired level of confidence
#' @return an estimated success rate (%) as a numeric
#' @family point estimate adjusters
#' @rdname wilson
#' @export
#'
#'
wilson <- function(.success, .trials, .Z=1.959964) {
    .W <- .Z ^ 2 #z critical 95% confidence interval
    .success <- (.W / 2) + .success
    .trials <- (.trials + .W)
    .out<- (.success / .trials)
    .out
  }
