#' Test two means against each other for a "between-subjects" design
#'
#' @description
#' \code{test_means_between()} compares two sample means obtained from two different groups of users  and performs a two-sided statistical test to determine if the means differ from each other.
#'
#' \code{test_means_between}, \code{ab_test_between}, and \code{test_times_between} are synonyms.
#'
#' @details
#' \itemize{
#'  \item If \code{.x} is a single numeric value representing the mean of the the variable for sample 1, use \code{.sd} and \code{.n} to provide the standard deviation and sample size. Use \code{.x2,.sd2,.n2} to provide the equivalent information for sample 2.
#'  \item If \code{.x} is a data frame with unique variable columns for each sample,\code{.var1} and \code{.var2} should be the unquoted names of the columns containing the values for sample 1 and sample 2
#'  \item If \code{.x} is a data frame with only one variable column for both samples, pass the unquoted column name to \code{.var1}, set \code{.var2 = NULL}, and use the grouping argument (\code{...}) to provide the name of the column that contains the sample id, which should only contain two unique values. Note that if you use this option, you cannot pass other grouping variables.  should be the unquoted names of the columns containing the values for sample 1 and sample 2
#'  \item You can modify the alpha level to adjust confidence intervals by including \code{.alpha} as a named argument and providing a numeric value: e.g., \code{.aplha = 0.001}.
#'  \item If you're passing a data frame to \code{.x}, you can optionally pass one or more grouping variables as unquoted, comma-separated column names (without naming the \code{...} argument) to compute stats by groups.
#' }
#'
#' Note that \code{NAs} are automatically dropped in all calculations.
#'
#'
#' @param .x A single numeric value (i.e., the mean from sample 1) or a long-format data frame with named columns of numeric data corresponding to the variables being tested. See Details.
#' @return A tibble with the results of a test of two means, with confidence interval information, and other information.
#' @family tests comparing means
#' @importFrom stats pt
#' @importFrom dplyr group_by group_modify summarise
#' @include between_df-function.R
#' @include between_diff_ci-function.R
#' @include tdist-function.R
#' @examples
#' # When you already have summary stats:
#'
#' test_means_between(.x=45,.sd = 9.2,.n=18,.x2=48, .sd2=8.5, .n2=17, .alpha=0.01)
#'
#'
#' @rdname test_means_between


#' @export
#'
#'
test_means_between <- function(.x, ...) {
  UseMethod("test_means_between", .x)
}
#' @rdname test_means_between
#' @export
ab_test_between <- test_means_between
#'
#' @rdname test_means_between
#' @param .sd If \code{.x} is a single numeric value representing the mean of the variable for sample 1, \code{.sd} should be a single numeric value representing the standard deviation of the variable for sample 1. See Details.
#' @param .n If \code{.x} is a single numeric value representing the mean of the variable for sample 1, \code{.sd} should be a single numeric value representing the total number of users in  sample 1. See Details.
#' @param .x2 If \code{.x} is a single numeric value representing the mean of the variable for sample 1,\code{.x2} should be a single numeric value representing the mean from sample 2.
#' @param .sd2 If \code{.x} is a single numeric value representing the mean of the variable for sample 1, \code{.sd2} should be a single numeric value representing the standard deviation of the variable for sample 2. See Details.
#' @param .n2 If \code{.x} is a single numeric value representing the mean of the variable for sample 1, \code{.sd} should be a single numeric value representing the total number of users in  sample 2. See Details.
#' @param ... (Optional) A positive number (where 0 < \code{.alpha} < 1) specifying the significance level to be used. Defaults to \code{.alpha = 0.05}. To set a different significance level, the argument must be named (i.e., \code{.alpha=0.001}) or else the function may yield unexpected results.
#' @param .alpha (Optional) A positive number (where 0 < \code{.alpha} < 1) specifying the significance level to be used. Defaults to \code{.alpha = 0.05}. To set a different significance level, the argument must be named (i.e., \code{.alpha=0.001}) or else the function may yield unexpected results.
#' @export
#'
test_means_between.numeric <- function(.x, .sd, .n, .x2,.sd2,.n2, ..., .alpha = .05) {
  if(length(.x)==1 && missing(.n)) {
    stop(
      "You need to specify .n as the size of sample 1"
    )
  }
  if(length(.x)==1 && missing(.sd)) {
    stop(
      "You need to specify .sd as the standard deviation for the variable from sample 1"
    )
  }
  if(length(.x)==1 && missing(.x2)) {
    stop(
      "You need to specify .x2 as the mean of the variable for sample 2"
    )
  }
  if(length(.x)==1 && missing(.sd2)) {
    stop(
      "You need to specify .sd as the standard deviation for the variable from sample 2"
    )
  }
  if(length(.x)==1 && missing(.n2)) {
    stop(
      "You need to specify .n2 as the size of sample 2"    )
  }


  if (.alpha < 0 | .alpha > 1) {
    stop(".alpha must be a positive number between 0 and 1")
  }


    .t_val<- (.x-.x2)/sqrt((.sd^2/.n) + (.sd2^2/.n2))
    .p_val<-tdist(.t_val,.df = between_df(.sd,.n,.sd2,.n2),.tail=2)
    .t_crit<-tinv(.alpha,.df = between_df(.sd,.n,.sd2,.n2),.tail=2)
    .ci_low <-between_diff_ci(.x, .sd, .n,.x2, .sd2, .n2, .t_crit, .return="low")
    .ci_hi <-between_diff_ci(.x, .sd, .n,.x2, .sd2, .n2, .t_crit, .return="hi")
    .ci_note <- paste0((1.0 -(.alpha))*100,"% CI for a between-subjects design")

    data.frame(
      "samp1_mean" = .x,
      "samp2_mean" = .x2,
      "samp1_stdev" = .sd,
      "samp2_stdev" = .sd2,
      "samp1_size" = .n,
      "samp2_size" = .n2,
      "t_val" = .t_val,
      "p_val" = .p_val,
      "ci_low"= .ci_low,
      "ci_hi"= .ci_hi,
      "ci_method" = .ci_note
    )

}



#' @rdname test_means_between
#' @param .var1 If \code{.x} is a long-format data frame, the (unquoted) name of a data frame column containing the values representing the first sample.
#' @param .var2 If \code{.x} is a long-format data frame, the (unquoted) name of a data frame column containing the values representing the second sample.
#' @param ... (Optional) If \code{.x} is a long-format data frame with a separate unique column of data for each sample, you can pass the name of one or more grouping variables as unquoted, comma-separated column names (without naming the \code{...} argument) to compute stats by groups.
#' @examples

#' # If you have a data frame with a separate column of observations
#' # for each sample:
#'
#' .ux_data<-
#'   data.frame("sample1_ratings" = runif(20,20,50),
#'              "sample2_ratings" = c(runif(15,20,50),rep(NA,times=5)))
#'
#' test_means_between(.ux_data, sample1_ratings, sample2_ratings, .alpha=0.10)
#'
#'
#' @export
#'
test_means_between.data.frame <- function(.x, .var1, .var2, ..., .alpha = 0.05) {
if (.alpha < 0 | .alpha > 1) {
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
      samp1_mean = mean({{ .var1}}, na.rm = TRUE),
      samp2_mean = mean({{ .var2}}, na.rm = TRUE),
      samp1_stdev = stats::sd({{ .var1}}, na.rm = TRUE),
      samp2_stdev = stats::sd({{ .var2}}, na.rm = TRUE),
      samp1_n = dplyr::n()- sum(is.na({{.var1}})),
      samp2_n = dplyr::n()- sum(is.na({{.var2}})),
      .groups = "keep"
    )

  .out <-
    dplyr::group_modify(.out,
                        ~ test_means_between.numeric(.x$samp1_mean,
                                                .x$samp1_stdev,
                                                .x$samp1_n,
                                                .x$samp2_mean,
                                                .x$samp2_stdev,
                                                .x$samp2_n,
                                                .alpha = .alpha),
                        .keep = TRUE)

  return(.out)
}


