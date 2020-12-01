#' Compute stats for task success (completion) data
#'
#' @description
#' \href{https://g.co/kgs/a7Zyyn}{Sauro and Lewis (2012)} describe various approaches for estimating success rates and generating confidence intervals when you're working with smaller sample sizes. \code{success_stats()} automatically determines which of several estimator adjustments is best suited to the data, and it returns a tibble with the original and adjusted success rates (as a percentage); a field to indicate which adjustment method was used; and information about the confidence interval.
#'
#' \code{success_stats()} and \code{completion_stats()} are synonyms.
#'
#' @details
#' \itemize{
#'   \item \code{.x} is the only required argument if you are passing a vector of 1s and 0s, representing successes and failures, respectively.  e.g., \code{.x = c(1,1,1,1,1,0,1)}
#'  \item \code{.x} is a single numeric value representing the total number of successes, \code{.n} should be a single numeric value representing the total number of trials (where the value of \code{.y} >= the value of \code{.x}). e.g., \code{.x = 23, .y = 25}
#'  \item \code{.x} is a data frame, \code{.var} should be the unquoted name of the column containing the success data (as 1s and 0s).
#'  \item You can modify the alpha level to adjust confidence intervals by including \code{.alpha} as a named argument and providing a numeric value: e.g., \code{.aplha = 0.001}.
#'  \item If you're passing a data frame to \code{.x}, you can optionally pass one or more grouping variables as unquoted, comma-separated column names (without naming the \code{...} argument) to compute stats by groups.
#' }
#'
#' Note that \code{NAs} are automatically dropped in all calculations.
#'
#'
#' @param .x A single numeric value, a vector of values, or a long-format data frame with a named column of numeric data (1s and/or 0s) corresponding to task success outcomes. See Details.
#' @return A tibble with success rate(s), confidence interval information, and other information. All percentage values in the output fall within the range of 0 and 100.
#' @family descriptive stats for UX measures
#' @importFrom stats qnorm
#' @importFrom dplyr n
#' @importFrom dplyr select
#' @importFrom dplyr group_by
#' @importFrom dplyr group_modify
#' @importFrom dplyr summarise
#' @include wilson-function.R
#' @include laplace-function.R
#' @include mle-function.R
#' @include adjwald_ci-function.R
#' @rdname success_stats
#' @examples
#' #You can pass a vector of 1s and 0s to .x:
#'
#' success_stats(c(1,1,1,1,0,0,1,1,0,1,0,1))
#'
#' # If you want a summary for a single task, you can provide the number
#' # of successes and trials to .x and .n, respectively:
#'
#' success_stats(.x = 15, .n = 20)
#'
#'
#' # You can pass a long-format data frame to .x and
#' # and specify the name of the appropriate column to .var:
#'
#' .ux_data <-
#'   data.frame(
#'    "id" = rep(seq(1,10,1),2),
#'    "group" = rep(c("A","B"),10),
#'    "task" = c(rep(1,10),rep(2,10)),
#'    "task_success"  = sample(0:1,20,replace=TRUE,prob = c(.3,.65)))
#'
#' success_stats(.ux_data, task_success)
#'
#' # If you have one or more grouping variables, pass them to the ... argument:
#'
#' success_stats(.ux_data, task_success, group, task)
#'
#' # .alpha defaults to 0.05. Change the value by
#' # naming the argument when you call the function:
#'
#' success_stats(15,20, .alpha = 0.01)

#' @export
#'
#'
success_stats <- function(.x, ...) {
  UseMethod("success_stats", .x)
}
#' @rdname success_stats
#' @export
completion_stats <- success_stats
#'
#'
#' @rdname success_stats
#' @param .n If \code{.x} is a single numeric value, \code{.n} should be a single numeric value representing the total number of trials. See Details.
#' @param .alpha (Optional) A positive number (where 0 < \code{.alpha} < 1) specifying the significance level to be used. Defaults to \code{.alpha = 0.05}. To set a different significance level, the argument must be named (i.e., \code{.alpha=0.001}) or else the function may yield unexpected results.
#' @export
#'
success_stats.numeric <- function(.x, .n = NULL, ..., .alpha = .05) {
if(length(.x)==1 && missing(.n)) {
    stop(
      "You need to specify .n as the total number of trials."
    )
  }
    if (.alpha < 0 | .alpha > 1) {
      stop(".alpha must be a positive integer between 0 and 1")
    }
    else {
      .Z <- stats::qnorm(1.0 - (.alpha / 2))
    }

  if(length(.x)== 1){
  .p <- .x / .n
  } else if (any(.x >1,na.rm = TRUE)){
    stop("If you're passing a vector of values, the vector should contain only 1s (for successes) and 0s (for failures).")
  } else{
    .n <-
      length(.x[!is.na(.x)])
    .x <-
      sum(.x, na.rm=TRUE)
    .p <- (.x/.n)
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
      .pout <- laplace(.success = .x, .trials = .n)
      .ci <-
        adjwald_ci(.success = .x,
                .trials = .n,
                .Z = .Z)
      .out <- list("=0", "Laplace", .pout, list(0, .ci[[2]]))
      .out
    }
    else if (.p == 1) {
      .pout <- laplace(.success = .x, .trials = .n)
      .ci <-
        adjwald_ci(.success = .x,
                .trials = .n,
                .Z = .Z)
      .out <- list("=1", "Laplace", .pout, list(.ci[[1]], 100))
      .out
    }
    else if (.p < .5 && .p != 0) {
      .pout <-
        wilson(.success = .x,
               .trials = .n,
               .Z = .Z)
      .ci <-
        adjwald_ci(.success = .x,
                .trials = .n,
                .Z = .Z)
      .out <- list("<.5", "Wilson", .pout, .ci)
      .out
    }
    else if (.p > .9 && .p != 0) {
      .pout <-
        laplace(.success = .x, .trials = .n)
      .ci <-
        adjwald_ci(.success = .x,
                .trials = .n,
                .Z = .Z)
      .out <- list("<.9", "Laplace", .pout, .ci)
      .out
    }
    else {
      .pout <-
        mle(.success = .x, .trials = .n)
      .ci <-
        adjwald_ci(.success = .x,
                .trials = .n,
                .Z = .Z)
      .out <- list(".5<p<.9", "MLE", .pout, .ci)
      .out
    }
    return(
      data.frame(
        "successes" = .x,
        "trials" = .n,
        "observed_success" = round(.p * 100, 2),
        "estimated_success" = round(.out[[3]] * 100, 2),
        "success_estimator" = .out[[2]],
        "ci_low" = round(.out[[4]][[1]] *100, 2),
        "ci_hi" =  round(.out[[4]][[2]] * 100, 2),
        "ci_method" = paste0((1.0-.alpha)*100,"% CI based on Adjusted Wald"),
        stringsAsFactors = FALSE
      )
    )

}

#' @rdname success_stats
#' @param .var If \code{.x} is a long-format data frame, the (unquoted) name of a data frame column containing task success outcomes (as 1s and 0s, corresponding to successes and failures, respectively).
#' @param ... (Optional) If \code{.x} is a long-format data frame, you can pass the name of one or more grouping variables here as unquoted, comma-separated column names.
#' @export
#'
success_stats.data.frame <- function(.x, .var, ..., .alpha = NULL) {
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
    dplyr::summarise(
      .out,
      trials = dplyr::n(),
      success = sum({{ .var }}),
      .groups = "keep"
    )
  .out <-
    dplyr::group_modify(.out,
                        ~ success_stats.numeric(.x$success,
                                                  .n = .x$trials,
                                                  .alpha = .alpha),
                        .keep = TRUE)

  return(.out)
}


