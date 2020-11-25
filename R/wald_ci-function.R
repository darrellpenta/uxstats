#' Computes confidence intervals using adjusted Wald (Agresti & Coull, 1998)
#'
#'
#' @param .success  the proportion of successes
#' @param .trials the total number of trials
#' @param .Z The Z score corresponding to the desired level of confidence
#' @return a list with the lower and upper confidence limits (%) as numerics
#' @family point estimate helpers
#' @rdname wald_ci
#' @export
#'
#'
wald_ci <-
  function(.success, .trials, .Z = 1.959964){
    .W <- .Z ^ 2
    .success <- (.W / 2) + .success
    .trials <- (.trials + .W)
    .p <- .success / .trials
    .nn <- .p * (1 - .p)
    .est <- sqrt((.nn / .trials))
    .z <- .Z * .est
    .p_hi <- .p + .z
    .p_lo <- .p - .z
    .p_hi <- ifelse(.p_hi >= 1,1,.p_hi)
    .p_lo <- ifelse(.p_lo <= 0,0,.p_lo)
    return(list(.p_lo, .p_hi))
  }
