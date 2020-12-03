#' Test two means against each other
#'
#' @description
#' \code{two_means_test()} compares to sample means and performs a two-sided statistical test to determine if the means differ from each other.
#'
#' \code{two_means_test}, \code{ab_test}, and \code{two_times_test} are synonyms.
#'
#' @details
#' \itemize{
#'  \item If \code{.x} is a single numeric value representing the mean of differences from the sample scores, \code{.sd} should be a single numeric value representing the standard deviation of the difference scores.
#'  \item If \code{.x} is a single numeric value representing the mean of differences from the sample scores, \code{.n} should be a single numeric value representing the number of pairs that derived the difference scores.
#'  \item If \code{.x} is a data frame, \code{.var1} and \code{.var2} should be the unquoted names of the columns containing the raw scores obtained.
#'  \item You can modify the alpha level to adjust confidence intervals by including \code{.alpha} as a named argument and providing a numeric value: e.g., \code{.aplha = 0.001}.
#'  \item If you're passing a data frame to \code{.x}, you can optionally pass one or more grouping variables as unquoted, comma-separated column names (without naming the \code{...} argument) to compute stats by groups.
#' }
#'
#' Note that \code{NAs} are automatically dropped in all calculations.
#'
#'
#' @param .x A single numeric value (i.e., the difference of two means) or a long-format data frame with named columns of numeric data corresponding to the variables being tested. See Details.
#' @return A tibble with the results of a test of two means, with confidence interval information, and other information.
#' @family tests comparing means
#' @importFrom stats pt
#' @importFrom dplyr group_by group_modify summarise
#' @include within_diff_ci-function.R
#' @include tdist-function.R
#' @examples
#' # When you have computed difference metrics in advance:
#' two_means_test(45,9.2,18)
#'
#' .ux_data <-
#'  data.frame(
#'   "id" = rep(seq(1,20,1),2),
#'   "task" = c(rep(1,20),rep(2,20)),
#'   "score1"  = sample(1:7,40,TRUE),
#'   "score2"  = sample(1.5:7.5,40,TRUE)
#'   )
#'
#' two_means_test(.ux_data, score1, score2, task, .alpha=0.1)
#' @rdname two_means_test


#' @export
#'
#'
two_means_test <- function(.x, ...) {
  UseMethod("two_means_test", .x)
}
#' @rdname two_means_test
#' @export
ab_test <- two_means_test
#'
#' @rdname two_means_test
#' @param .sd If \code{.x} is a single numeric value representing the mean of the variable differences, \code{.sd} should be a single numeric value representing the standard deviation of the diferences. See Details.
#' @param .n If \code{.x} is a single numeric value representing the mean of the variable differences, \code{.sd} should be a single numeric value representing the total number of trials. See Details.
#' @param ... (Optional) A positive number (where 0 < \code{.alpha} < 1) specifying the significance level to be used. Defaults to \code{.alpha = 0.05}. To set a different significance level, the argument must be named (i.e., \code{.alpha=0.001}) or else the function may yield unexpected results.
#' @param .alpha (Optional) A positive number (where 0 < \code{.alpha} < 1) specifying the significance level to be used. Defaults to \code{.alpha = 0.05}. To set a different significance level, the argument must be named (i.e., \code{.alpha=0.001}) or else the function may yield unexpected results.
#' @export
#'
two_means_test.numeric <- function(.x, .sd, .n, ..., .alpha = .05) {
  if(length(.x)==1 && missing(.n)) {
    stop(
      "You need to specify .n as the total number of mean pairs"
    )
  }
  if(length(.x)==1 && missing(.sd)) {
    stop(
      "You need to specify .sd as the standard deviation of the mean differences"
    )
  }
  if (.alpha < 0 | .alpha > 1) {
    stop(".alpha must be a positive number between 0 and 1")
  }


  if(length(.x)== 1){
    .t_val<- .x/(.sd/sqrt(.n))
    .p_val<-tdist(.t_val,.df = (.n-1),.tail=2)
    .ci_low <-within_diff_ci(.x, .sd, .n, .alpha, .return="low")
    .ci_hi <-within_diff_ci(.x, .sd, .n, .alpha, .return="hi")
    .ci_note <- paste0((1.0 -(.alpha))*100,"% CI for a within-subjects difference in means")

    data.frame(
      "diff_mean" = .x,
      "diff_stdev" = .sd,
      "sample_size" = .n,
      "t_val" = .t_val,
      "p_val" = .p_val,
      "ci_low"= .ci_low,
      "ci_hi"= .ci_hi,
      "ci_method" = .ci_note
    )

}
}


#' @rdname two_means_test
#' @param .var1 If \code{.x} is a long-format data frame, the (unquoted) name of a data frame column containing the values for computing the first mean.
#' @param .var2 If \code{.x} is a long-format data frame, the (unquoted) name of a data frame column containing the values for computing the first mean.
#' @param ... (Optional) If \code{.x} is a long-format data frame, you can pass the name of one or more grouping variables as unquoted, comma-separated column names (without naming the \code{...} argument) to compute stats by groups.
#' @export
#'
two_means_test.data.frame <- function(.x, .var1, .var2, ..., .alpha = 0.05) {
if (.alpha < 0 | .alpha > 1) {
    stop(".alpha must be a positive integer between 0 and 1")
  }
  else {
    .alpha <- .alpha
  }
  .out <-
    dplyr::group_by(.x, ...)
  # .out <-
  #   dplyr::select(.out, `var1` = {{ .var1 }}, `var2` = {{ .var2 }})
  # .out$diff <-.out$var1 - .out$var2
  .out <-
    dplyr::summarise(
      .out,
      diff_mean = mean({{ .var1}}, na.rm = TRUE) - mean( {{.var2}}),
      diff_stdev = stats::sd({{ .var1}} - {{.var2}}, na.rm = TRUE),
      n = dplyr::n(),
      .groups = "keep"
    )
  .out <-
    dplyr::group_modify(.out,
                        ~ two_means_test.numeric(.x$diff_mean,
                                                .x$diff_stdev,
                                                .n = .x$n,
                                                .alpha = .alpha),
                        .keep = TRUE)

  return(.out)
}


