#' Compute stats for task success (completion) data
#'
#' \href{https://g.co/kgs/a7Zyyn}{Sauro and Lewis (2012)} describe various approaches for estimating success rates and generating confidence intervals when you're working with smaller sample sizes. \code{success_stats()} automatically determines which of several estimator adjustments is best suited to the data, and it returns a tibble with the original and adjusted success rates (as a percentage); a field to indicate which adjustment method was used; and information about the confidence interval. You can optionally include one or more grouping variables to compute success rates by groups, and modify the alpha level to adjust confidence intervals.
#' `success_stats()` and `completion_stats()` are synonyms.
#'
#' @param .x You can pass an integer (>0) indicating the total number of successes, and provide the total number of trials  to \code{.y} (where the value of \code{.y} >= \code{.x}). Or you can pass a data frame containing trial data. See the examples below.
#' @param .y If \code{.x} is an integer representing the total number of successes, \code{.y} should be an integer indicating the total number of trials. Or, if \code{.x} is a long-format data frame, provide the unquoted name of the column containing the success data (as 1s and 0s) to \code{.y}.
#' @param ... (Optional) If \code{.x} is a long-format data frame, you can pass the name of one or more grouping variables here as unquoted, comma-separated column names.
#' @return A tibble with success rate(s), confidence interval information, and other information. All percentage values in the output fall within the range of 0 and 100.
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
#' @include adjwald_ci-function.R
#' @rdname success_stats
#' @examples
#' # If you want a summary for a single task,
#' # you can provide the number of successes
#' # and trials to .x and .y, respectively:
#'
#' success_stats(15,20)
#'
#' # The alpha level defaults to .alpha=0.05.
#' # You can provide your own alpha level
#' # to .alpha by naming the argument
#' # when you call the function:
#'
#' success_stats(15,20, .alpha = 0.01)
#'
#' # If you have a long-format data frame,
#' # where each row contains an individual's
#' # data, and at least one column includes
#' # success (as 1s) or failure (as 0s) values
#' # for a given task, you can pass the
#' # data frame to .x and specify the name
#' # of the task column:
#'
#' .uxdata <-
#' data.frame("user_id" = c(1,2,3,4,5,6,7,8,9,10,11,12),
#' "task1" = c(1,0,0,1,0,0,1,1,0,0,1,0),
#' "group"=c("A","B","A","A","B","A","B","A","B","B","A","B"),
#' "version"=c(2,1,1,2,1,2,2,1,2,1,1,2),
#' stringsAsFactors = FALSE)
#'
#' success_stats(.uxdata, task1)
#'
#' # If you have one or more grouping variables,
#' # you pass them to the ... argument:
#' success_stats(.uxdata, task1, group, version, .alpha=0.1)
#'

#' @export
#'
#'
success_stats <- function(.x, .y, ...) {
  UseMethod("success_stats", .x)
}
#' @rdname success_stats
#' @export
completion_stats <- success_stats
#'
#'
#' @rdname success_stats
#' @param .alpha (Optional) A positive number (where 0 < \code{.alpha} < 1) specifying the desired confidence level to be used. The argument must be named (i.e., \code{.alpha=0.001}) or else the function may yield unexpected results. If the argument is omitted, the default value is 0.05, or a 95\% confidence level.
#' @export
#'
success_stats.numeric <- function(.x, .y, ..., .alpha = NULL) {
  if (is.data.frame(.x)) {
    NextMethod("success_stats")
  }
  else if (is.numeric(.x) && missing(.y)) {
    stop(
      ".x should be a numeric representing the number of successes. You need to specify .y as the number of trials."
    )
  }
  else{
    .p <- .x / .y
    if (missing(.alpha)) {
      .Z <- stats::qnorm(1 - (0.05 / 2))
    }
    else if (.alpha < 0 | .alpha > 1) {
      stop(".alpha must be a positive integer between 0 and 1")
    }
    else {
      .Z <- stats::qnorm(1 - (.alpha / 2))
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
        adjwald_ci(.success = .x,
                .trials = .y,
                .Z = .Z)
      .out <- list("=0", "Laplace", .pout, list(0, .ci[[2]]))
      .out
    }

    else if (.p == 1) {
      .pout <- laplace(.success = .x, .trials = .y)
      .ci <-
        adjwald_ci(.success = .x,
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
        adjwald_ci(.success = .x,
                .trials = .y,
                .Z = .Z)
      .out <- list("<.5", "Wilson", .pout, .ci)
      .out
    }

    else if (.p > .9 && .p != 0) {
      .pout <-
        laplace(.success = .x, .trials = .y)
      .ci <-
        adjwald_ci(.success = .x,
                .trials = .y,
                .Z = .Z)
      .out <- list("<.9", "Laplace", .pout, .ci)
      .out
    }
    else {
      .pout <-
        mle(.success = .x, .trials = .y)
      .ci <-
        adjwald_ci(.success = .x,
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
        "ci_low_pct" = round(.out[[4]][[1]] *100, 2),
        "ci_hi_pct" =  round(.out[[4]][[2]] * 100, 2),
        stringsAsFactors = FALSE
      )
    )
  }
}


#' @rdname success_stats
#' @export
#'
success_stats.data.frame <- function(.x, .y, ..., .alpha = NULL) {
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
                        ~ success_stats.numeric(.x$success,
                                                  .y = .x$trials,
                                                  .alpha = .alpha),
                        .keep = TRUE)

  return(.out)
}


