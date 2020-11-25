#' Computes estimated success rate with confidence intervals for small samples, following Sauro and Lewis (2012)
#'
#' @param .x You can pass an integer (>0) indicating the total number of successes, and provide the total number of trials  to \code{.y} (where the value provided to \code{.y} > \code{.y}). Or you can pass a data frame containing trial data.
#' @param .y If \code{.x} is an integer representing the total number of successes, \code{.y} should be an integer indicating the total number of trials. Or, if \code{.x} is a data frame, provide the unquoted name of the column containing the success data (as 1s and 0s) to \code{.y}.
#' @param ... (Optional) If \code{.x} is a data frame, you can pass the name of grouping variables here as unquote, comma-separated column names.
#' @return a tibble with
#' @family success rate estimators
#' @importFrom stats qnorm
#' @importFrom dplyr n
#' @importFrom dplyr select
#' @importFrom dplyr mutate
#' @importFrom dplyr across
#' @importFrom dplyr group_by
#' @importFrom dplyr group_modify
#' @importFrom dplyr summarise
#' @include wilson-function.R
#' @include laplace-function.R
#' @include mle-function.R
#' @include wald_ci-function.R
#' @rdname ux_success_rate
#' @export
#'
#'
ux_success_rate <- function(.x, .y, ...) {
  UseMethod("ux_success_rate", .x)
}

#' @rdname ux_success_rate
#' @export
#'
ux_success_rate.default <- function(.x, .y, ...) {
  NextMethod("ux_success_rate")
}

#' @rdname ux_success_rate
#' @param .alpha (Optional) A positive number (where 0 < \code{.alpha} <1) specifying the desired confidence level to be used. The argument must be named (i.e., \code{.alpha=0.001}) or else the function may yield unexpected results. If the argument is omitted, it defaults to 0.05.
#' @export
#'
ux_success_rate.numeric <- function(.x, .y, ..., .alpha = NULL) {
  if (is.data.frame(.x)) {
    NextMethod("ux_success_rate")
  }
  else if (is.numeric(.x) && missing(.y)) {
    stop(
      ".x should be a numeric representing the number of successes. You need to specify .y as the number of trials."
    )
  }
  else{
    .p <- .x / .y
    if (missing(.alpha)) {
      .Z <- stats::qnorm(1 - 0.05 / 2)
    }
    else if (.alpha < 0 | .alpha > 1) {
      stop(".alpha must be a positive integer between 0 and 1")
    }
    else {
      .Z <- .Z <- stats::qnorm(1 - .alpha / 2)
    }

    if (.p > 1) {
      return("STOP! Check your calculations; rate is greater than 100")
      stop()
    }
    else if (.p < 0) {
      return("STOP! Check your calculations; rate is less than 0")
      stop()
    }
    else if (.p == 0) {
      .pout <- laplace(.success = .x, .trials = .y)
      .ci <-
        wald_ci(.success = .x,
                .trials = .y,
                .Z = .Z)
      .out <- list("=0", "Laplace", .pout, list(0, .ci[[2]]))
      .out
    }

    else if (.p == 1) {
      .pout <- laplace(.success = .x, .trials = .y)
      .ci <-
        wald_ci(.success = .x,
                .trials = .y,
                .Z = .Z)
      .out <- list("=1", "Laplace", .pout, list(.ci[[1]], 100))
      .out
    }
    else if (.p < .5 && .p != 0) {
      .pout <-
        wilson(.success = .x,
               .trials = .y,
               .Z = .Z)
      .ci <-
        wald_ci(.success = .x,
                .trials = .y,
                .Z = .Z)
      .out <- list("<.5", "Wilson", .pout, .ci)
      .out
    }

    else if (.p > .9 && .p != 0) {
      .pout <-
        laplace(.success = .x, .trials = .y)
      .ci <-
        wald_ci(.success = .x,
                .trials = .y,
                .Z = .Z)
      .out <- list("<.9", "Laplace", .pout, .ci)
      .out
    }
    else {
      .pout <-
        mle(.success = .x, .trials = .y)
      .ci <-
        wald_ci(.success = .x,
                .trials = .y,
                .Z = .Z)
      .out <- list(".5<p<.9", "MLE", .pout, .ci)
      .out
    }
    return(
      data.frame(
        "successes" = .x,
        "trials" = .y,
        "orig_succ_pct" = round(.p * 100, 2),
        "estimator" = .out[[2]],
        "success_pct" = round(.out[[3]] * 100, 2),
        "low_ci_pct" = round(.out[[4]][[1]] *100, 2),
        "hi_ci_pct" =  round(.out[[4]][[2]] * 100, 2),
        stringsAsFactors = FALSE
      )
    )
  }
}


#' @rdname ux_success_rate
#' @export
#'
ux_success_rate.data.frame <- function(.x, .y, ..., .alpha = NULL) {
  if (missing(.alpha)) {
    .alpha <- 0.05
  }
  else if (.alpha < 0 | .alpha > 1) {
    stop(".alpha must be a positive integer between 0 and 1")
  }
  else {
    .alpha <- .alpha
  }
  .out <-
    dplyr::group_by(.x, ...)
  .out <-
    dplyr::mutate(.out, dplyr::across({{ .y }}, as.numeric))
  .out <-
    dplyr::summarise(
      .out,
      trials = dplyr::n(),
      success = sum({{ .y }}),
      .groups = "keep"
    )
  .out <-
    dplyr::group_modify(.out,
                        ~ ux_success_rate.numeric(.x$success,
                                                  .y = .x$trials,
                                                  .alpha = .alpha),
                        .keep = TRUE)

  return(.out)
}
