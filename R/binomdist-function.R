#' Find a binomial-distribution probability
#'
#' @description
#' Similar to Excel's \code{BINOMDIST(number_s,trials,probability_s,cumulative)} function.
#' \code{binomdist()} uses \code{stats::pbinom()} when \code{.cumulative = TRUE}, and \code{stats::dbinom()} when not (i.e., the default).
#'
#'
#' @param .s  The number of successes. Cannot be less than 0.
#' @param .t The number of trials. Must be greater than 0 and greater than or equal to \code{.s}
#' @param .p The probability of success on each trial. Must be greater than 0 and less than 1.
#' @param .cumulative Defaults to FALSE, returning the probability mass function, which is the probability that there are \code{.s} successes. If TRUE, \code{binomdist} returns the cumulative distribution function, which is the probability that there are at most \code{.s} successes.
#' @param ... (Optional) Extra arguments passed to \code{stats::pbinom()} or \code{stats::dbinom()}
#' @return Returns the individual term binomial distribution probability.
#' @importFrom stats pbinom dbinom
#' @family binomial-distribution functions (~ Excel)
#' @rdname binomdist
#' @export
#'
binomdist<-
  function(.s, .t, .p, .cumulative = FALSE,...){

  if(.s < 0){
    stop(".s cannot be less to zero")
  } else if(.t <= 0){
    stop(".t cannot be equal to zero")
  }
  if(.s > .t){
    stop(".s cannot be gerater than .t")
  }
if(.cumulative == FALSE){
return(dbinom(.s,.t,.p,...))
  }
if(.cumulative == TRUE){
return(pbinom(.s,.t,.p,...))
  }
}
