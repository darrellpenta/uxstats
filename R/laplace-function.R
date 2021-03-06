#' Computes success rate using Laplace estimate
#'
#'
#' @param .success  the total number of successes
#' @param .trials  the total number of trials
#' @param ...  Arguments passed to or from other methods
#' @return an estimated success rate (%) as a numeric
#' @family point estimate adjusters

#' @rdname laplace
#' @export
#'
laplace <- function(.success, ...) {
  UseMethod("laplace", .success)
}


#' @rdname laplace
#' @export
#'
#'
laplace.numeric <- function(.success,.trials,...) {
    .success <- .success + 1
    .trials <- .trials + 2
    .out<-
      (.success / .trials)
    .out
  }
