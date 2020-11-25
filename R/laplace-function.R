#' Computes success rate using Laplace estimate
#'
#'
#' @param .success  the total number of successes
#' @param .trials  the total number of trials
#' @return an estimated success rate (%) as a numeric
#' @family point estimate helpers
#' @rdname laplace
#' @export
#'
#'
laplace <- function(.success,.trials) {
    .success <- .success + 1
    .trials <- .trials + 2
    .out<-
      (.success / .trials)
    .out
  }
