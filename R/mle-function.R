#' Computes success rate using Maximum Liklihood estimate
#'
#'
#' @param .success  the total number of successes
#' @param .trials the total number of trials
#' @return an estimated success rate (%) as a numeric
#' @family point estimate adjusters
#' @rdname mle
#' @export
#'
#'

mle <-
  function(.success, .trials){
    .out<-
      (.success / .trials)
    .out
  }
