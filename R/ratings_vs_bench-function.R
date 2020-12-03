#' Compare ratings to a benchmark
#'
#' @description
#' \code{ratings_vs_bench()} tests a sample mean of continuous data (e.g., ratings)  against a given benchmark using a one-sample t-test.
#'
#' @details
#' \code{ratings_vs_bench} assumes that you want to test the hypothesis that the observed outcome \emph{exceeds} the benchmark, and therefore, defaults to a one-tailed test. This means that setting \code{.alpha = 0.05} (the default) produces a 90\% confidence interval.
#'
#' \itemize{
#'  \item If \code{.x} is a single numeric value representing the sample mean, provide the sample standard deviation and sample size to \code{.sd} and \code{.n}, respectively.
#'  \item If \code{.x} is a numeric vector of ratings values, you should only specify the test (benchmark) mean.
#'  \item If \code{.x} is a data frame, \code{.var} should be the unquoted name of the column containing the ratings values.
#'  \item If you're passing a data frame to \code{.x}, you can optionally pass one or more grouping variables as unquoted, comma-separated column names (without naming the \code{...} argument) to compute stats by groups.
#'   \item You can choose from among the test alternatives \code{c("greater","less","twotailed")} by providing one of the options to the \code{.alt} argument: e.g., \code{.alt = "twotailed"}. Defaults to "greater" for a one-sided test.
#'  \item You can modify the alpha level to adjust confidence intervals by including \code{.alpha} as a named argument and providing a numeric value: e.g., \code{.aplha = 0.001}.
#' }
#'
#' Note that \code{NAs} are automatically dropped in all calculations.
#'
#' @param .x  A single numeric value, a vector of values, or a long-format data frame with a named column of numeric data corresponding to ratings or another continuous metric. See Details.
#' @param .sd If \code{.x} is a single numeric value, \code{.sd} should be a single numeric value indicating the sample standard deviation. See Details
#' @param .n  If \code{.x} is a single numeric value, \code{.n} should be a single numeric value indicating the sample size. See Details.
#' @param .m  The test (benchmark) mean.
#' @param .alt (Optional) For test alternatives, one of \code{c("greater","less","twotailed")}. Defaults to "greater" for a one-sided test.
#' @param .alpha (Optional) A positive number (where 0 < \code{.alpha} < 1) specifying the significance level to be used. Defaults to \code{.alpha = 0.05}. To set a different significance level, the argument must be named (i.e., \code{.alpha=0.001}) or else the function may yield unexpected results.
#' @return A tibble with data summaries and test results
#' @family benchmark comparison stats
#' @importFrom stats qnorm pt qt
#' @importFrom dplyr n group_by group_modify summarise
#' @include adjwald_ci-function.R
#' @include laplace-function.R
#' @include tdist-function.R
#' @include tinv-function.R
#' @include tdist_ci-function.R
#' @examples
#' # Comparing a sample ratings mean of 79 (sd = 20) from 160 users
#' # against a benchmark mean of 75
#' ratings_vs_bench(.x=79,.sd=20,.n=160,.m=75)
#'
#' # Comparing values from a data frame against a benchmark of 70.
#' .ux_data <-
#'  data.frame(
#'   "id" = rep(seq(1,10,1),2),
#'   "task" = c(rep(1,10),rep(2,10)),
#'   "ratings"  = runif(20,0,100))
#'
#' ratings_vs_bench(.ux_data, ratings, .m=70, task)
#'
#' @rdname ratings_vs_bench
#' @export
#'
ratings_vs_bench <- function(.x, ...) {
  UseMethod("ratings_vs_bench", .x)
}

#' @rdname ratings_vs_bench
#' @export

ratings_vs_bench.numeric <- function(.x,.sd,.n,.m,...,.alt = c("greater","less","twotailed"),.alpha=0.05){
  if (.alpha < 0 | .alpha > 1) {
    stop(".alpha must be a positive integer between 0 and 1")
  }
  if(missing(.m)){
    stop(
      "You need to specify .m as the test (benchmark) mean."
    )
  }
  if(length(.x)==1 && missing(.n)) {
    stop(
      "You need to specify .n as the total number of users (i.e., the sample size)."
    )
  }
  if(length(.x)==1 && missing(.sd)) {
    stop(
      "You need to specify .sd as sample standard deviation."
    )
  }

  .alt<-match.arg(.alt)
  if(.alt == "greater"){
    .Z <- stats::qnorm((1.0 -.alpha))
    .ci_note <- paste0((1.0 -(2*.alpha))*100,"% CI from a one-sided test based on the T distribution")
  }
  else if(.alt == "less"){
    .Z <- stats::qnorm(.alpha)
    .ci_note <- paste0((1.0 -(2*.alpha))*100,"% CI from a one-sided test based on the T distribution")
  }
  else if(.alt == "twotailed"){
    .Z <- stats::qnorm(1.0 - (.alpha/2))
    .ci_note <- paste0((1.0 -(.alpha))*100,"% CI from a one-sided test based on the T distribution")
  }
  else {
    stop("Something went wrong. Did you provide the correct argument to .alt?")
  }

  if(length(.x) > 1){
    .sd <- stats::sd(.x, na.rm=TRUE)
    .n <-
      length(.x[!is.na(.x)])
    .x <- mean(.x, na.rm = TRUE)
  }

.t_val <-
  (.x-.m)/(.sd/sqrt(.n))
.prob<-
  tdist(.t_val,.df=(.n-1),.tail=1)
.t_crit <-
  tinv(.alpha, .df=(.n-1), .tail = 1)
# .ci_low <- .x-(.t_crit*(.sd/sqrt(.n)))
# .ci_hi <- .x+(.t_crit*(.sd/sqrt(.n)))
return(
  data.frame(
    "sample_mean" = .x,
    "sample_stdev" = .sd,
    "sample_size" = .n,
    "test_mean" = .m,
    "pval" = .prob,
    "critical_t" = .t_crit,
    "ci_low" = tdist_ci(.x,.sd,.n,.t_crit,"low"),
    "ci_hi" = tdist_ci(.x,.sd,.n,.t_crit,"hi"),
    "ci_method" = .ci_note
  ))
}

#' @rdname ratings_vs_bench
#' @param .var If \code{.x} is a long-format data frame, the (unquoted) name of a data frame column containing task ratings (or values from another continuous metric).
#' @param ... (Optional) If \code{.x} is a long-format data frame, you can pass the name of one or more grouping variables as unquoted, comma-separated column names (without naming the \code{...} argument) to compute stats by groups.
#' @export
#'
ratings_vs_bench.data.frame<-function(.x,.var,.m,...,.alt="greater",.alpha=0.05){
  .out <-
    dplyr::group_by(.x, ...)
  .out <-
    dplyr::summarise(
      .out,
      mean = mean({{ .var }}, na.rm = TRUE),
      stdev = stats::sd({{ .var }}, na.rm = TRUE),
      n = dplyr::n(),
      .groups = "keep")

  .out <-
    dplyr::group_modify(.out,
                        ~ ratings_vs_bench.numeric(.x=.x$mean,
                                                   .sd = .x$stdev,
                                                   .n = .x$n,
                                                   .m = .m,
                                                   .alt = .alt,
                                                   .alpha = .alpha),
                        .keep = TRUE)

  return(.out)

}
