#' Compute stats for problem occurrence data
#'
#' \code{problem_stats()} provides information about problem occurrence data, including problem occurrence rates (as percentages) and (Adjusted-Wald binomial) confidence interval information (as percentages). You can optionally include one or more grouping variables to compute problem occurrence rates by groups, and modify the alpha level to adjust confidence intervals.
#'
#' @param .x You can pass an integer (>0) indicating the total number of users who encountered a problem, and provide the total number of users to \code{.y} (where the value of \code{.y} >= \code{.x}). Or you can pass a data frame containing similar data. See the examples below.
#' @param .y If \code{.x} is an integer representing the total number of successes, \code{.y} should be an integer indicating the total number of trials. Or, if \code{.x} is a long-format data frame, provide the unquoted name of the column containing the success data (as 1s and 0s) to \code{.y}.
#' @param ... (Optional) If \code{.x} is a long-format data frame, you can pass the name of one or more grouping variables here as unquoted, comma-separated column names.
#' @return A tibble with problem occurrence rate(s), confidence interval information, and other information. All percentage values in the output fall within the range of 0 and 100.
#' @family descriptive stats for UX measures
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
#' @rdname problem_stats
#' @examples
#'
#' problem_stats(5,8) # Five users encountered a problem, 8 did not.
#'
#' # The alpha level defaults to .alpha=0.05.
#' # You can provide your own alpha level
#' # to .alpha by naming the argument
#' # when you call the function:
#'
#' problem_stats(15,20, .alpha = 0.01)
#'
#' # If you have a long-format data frame,
#' # where each row contains an individual's
#' # data, and at least one column includes
#' # values indicating problem occurrences
#' # as 1s (problem encountered) and 0s
#' # you can pass the data frame to .x and
#' # specify the name of the task column:

#'
#' .uxdata <-
#' data.frame("user_id" = c(1,2,3,4,5,6,7,8,9,10,11,12),
#' "login_prob" = c(1,0,0,1,0,0,1,1,0,0,1,0),
#' "group"=c("A","B","A","A","B","A","B","A","B","B","A","B"),
#' "version"=c(2,1,1,2,1,2,2,1,2,1,1,2),
#' stringsAsFactors = FALSE)
#'
#' problem_stats(.uxdata, login_prob, group, .alpha=0.1)


#' @export
#'
#'
problem_stats <- function(.x, .y, ...) {
  UseMethod("problem_stats", .x)
}
#'
#' @rdname problem_stats
#' @param .alpha (Optional) A positive number (where 0 < \code{.alpha} < 1) specifying the desired confidence level to be used. The argument must be named (i.e., \code{.alpha=0.001}) or else the function may yield unexpected results. If the argument is omitted, the default value is 0.05, or a 95\% confidence level.
#' @export
#'
problem_stats.numeric <- function(.x, .y, ..., .alpha = NULL) {
  if (is.data.frame(.x)) {
    NextMethod("problem_stats")
  }
  else if (is.numeric(.x) && missing(.y)) {
    stop(
      ".x should be a numeric representing the number of times a problem was observed. You need to specify .y as the number of users."
    )
  }
  else{
    .p <- .x / .y
    if (missing(.alpha)) {
      .Z <- stats::qnorm(1.0 - (0.05 / 2))
    }
    else if (.alpha < 0 | .alpha > 1) {
      stop(".alpha must be a positive integer between 0 and 1")
    }
    else {
      .Z <- .Z <- stats::qnorm(1.0 - (.alpha / 2))
    }

    if (.p > 1) {
      return("STOP! Check your calculations; rate is greater than 100")
      stop()
    }
    else if (.p < 0) {
      return("STOP! Check your calculations; rate is less than 0")
      stop()
    }

      .pout <-
        mle(.success = .x, .trials = .y)
      .ci <-
        adjwald_ci(.success = .x,
                .trials = .y,
                .Z = .Z)
      .out <- list(".5<p<.9", "MLE", .pout, .ci)

    return(
      data.frame(
        "problems_observed" = .x,
        "all_observed" = .y,
        "occurrence_rate_pct" = round(.p * 100, 2),
        "ci_low_pct" = round(.out[[4]][[1]] *100, 2),
        "ci_hi_pct" =  round(.out[[4]][[2]] * 100, 2),
        stringsAsFactors = FALSE
      )
    )
  }
}


#' @rdname problem_stats
#' @export
#'
problem_stats.data.frame <- function(.x, .y, ..., .alpha = NULL) {
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
                        ~ problem_stats.numeric(.x$success,
                                               .y = .x$trials,
                                               .alpha = .alpha),
                        .keep = TRUE)

  return(.out)
}


