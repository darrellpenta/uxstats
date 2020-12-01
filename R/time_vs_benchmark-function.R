#' Compare time data to a benchmark
#'
#' @description
#' \code{time_vs_bench()} tests a sample mean of time data (e.g., ratings)  against a given benchmark using a one-sample t-test.
#'
#' @details
#' Following \href{https://g.co/kgs/a7Zyyn}{Sauro and Lewis (2012)},\code{ratings_vs_bench} log transforms the time data in calculations when \code{.x} is a vector or data frame of raw times.It also log-transforms the benchmark mean in all calculations. If you pass a single numeric value to \code{.x} as a mean of sample times, be sure to log transform the mean and provide the standard deviation of the log-transformed times to the \code{.sd} argument.
#' \code{ratings_vs_bench} assumes that you want to test the hypothesis that the observed outcome \emph{does not exceed} the benchmark, and therefore, defaults to a one-tailed test. This means that setting \code{.alpha = 0.05} (the default) produces a 90\% confidence interval.
#'
#' \itemize{
#'  \item If \code{.x} is a single numeric value representing the log-transformed sample mean, provide the standard deviation of the log-transformed times and sample size to \code{.sd} and \code{.n}, respectively.
#'  \item If \code{.x} is a numeric vector of ratings values, you should only specify the test (benchmark) mean.
#'  \item If \code{.x} is a data frame, \code{.var} should be the unquoted name of the column containing the time values.
#'  \item If you're passing a data frame to \code{.x}, you can optionally pass one or more grouping variables as unquoted, comma-separated column names (without naming the \code{...} argument) to compute stats by groups.
#'  \item You can choose from among the test alternatives \code{c("greater","less","twotailed")} by providing one of the options to the \code{.alt} argument: e.g., \code{.alt = "twotailed"}. Defaults to "greater" for a one-sided test.
#'  \item You can modify the alpha level to adjust confidence intervals by including \code{.alpha} as a named argument and providing a numeric value: e.g., \code{.aplha = 0.001}.
#' }
#'
#' Note that \code{NAs} are automatically dropped in all calculations.
#'
#' @param .x  A single numeric value, a vector of values, or a long-format data frame with a named column of numeric data corresponding to time values. See Details.
#' @param .sd If \code{.x} is a single numeric value, \code{.sd} should be a single numeric value indicating the standard deviation of the log-transformed sample means. See Details
#' @param .n  If \code{.x} is a single numeric value, \code{.n} should be a single numeric value indicating the sample size. See Details.
#' @param .m  The test (benchmark) mean.
#' @param .alt (Optional) For test alternatives, one of \code{c("greater","less","twotailed")}. Defaults to "greater" for a one-sided test.
#' @param .alpha (Optional) A positive number (where 0 < \code{.alpha} < 1) specifying the significance level to be used. Defaults to \code{.alpha = 0.05}. To set a different significance level, the argument must be named (i.e., \code{.alpha=0.001}) or else the function may yield unexpected results.
#' @return A tibble with data summaries and test results
#' @family benchmark comparison stats
#' @importFrom stats qnorm pt qt median
#' @importFrom rlang .data
#' @importFrom dplyr n group_by group_modify summarise mutate
#' @include tdist_ci-function.R
#' @examples
#' # When passing individual values, make sure the mean and sd are log-transformed
#' .sample_mean <- 45
#' .sample_sd <- 12
#' time_vs_bench(.x=log(.sample_mean), .sd=log(.sample_sd), .n=20, .m = 60,.alt="less")
#'
#' # You can pass a vector of raw (untransformed) times to .x:
#' time_vs_bench(.x=c(350,255,400,343,330,420), .m=375, .alt="less", .alpha=0.10)
#'
#'
#' .ux_data <-
#' data.frame(
#'  "id" = rep(seq(1,10,1),2),
#'  "group" = rep(c("A","B"),10),
#'  "task" = c(rep(1,10),rep(2,10)),
#'  "time"  = runif(20,200,1000)
#' )
#'
#' time_vs_bench(.ux_data, .var=time,.m=600,task,.alt="less", .alpha=0.05 )
#' @rdname time_vs_bench
#' @export
#'
time_vs_bench <- function(.x, ...) {
  UseMethod("time_vs_bench", .x)
}

#' @rdname time_vs_bench
#' @export

time_vs_bench.numeric <- function(.x,.sd,.n,.m,...,.alt = c("greater","less","twotailed"),.alpha=0.05){
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
      "You need to specify .sd as the standard deviation."
    )
  }

  .alt<-match.arg(.alt)
  if(.alt == "greater"){
    .ci_note <- paste0((1.0 -(2*.alpha))*100,"% CI from a one-sided test based on the T distribution")
    .alpha <- (1.0 -.alpha)
  }
  else if(.alt == "less"){
    .ci_note <- paste0((1.0 -(2*.alpha))*100,"% CI from a one-sided test based on the T distribution")
    .alpha <- .alpha
  }
  else if(.alt == "twotailed"){
    .ci_note <- paste0((1.0 -(.alpha))*100,"% CI from a one-sided test based on the T distribution")
    .alpha <- (1.0 - (.alpha/2))
  }
  else {
    stop("Something went wrong. Did you provide the correct argument to .alt?")
  }

  if(length(.x) == 1){
    .sample_sd <- NA_integer_
    .sample_mean <- NA_integer_
    .median <- NA_integer_
  } else if(length(.x) > 1){
    .sd <- stats::sd(log(.x), na.rm=TRUE)
    .sample_sd <- stats::sd(.x, na.rm=TRUE)
    .n <-
      length(.x[!is.na(.x)])
    .sample_mean <- mean(.x, na.rm = TRUE)
    .median <- stats::median(.x, na.rm = TRUE)
    .x <- mean(log(.x), na.rm = TRUE)
  } else{
    stop("Did you mean to pass a numeric vector or dataframe to .x?")
  }
  .raw_m <- .m
  .m <-log(.m)

  .t_val<-(.m-.x)/(.sd/sqrt(.n))
   .p_val<-
    pt(q = .t_val, df=(.n-1), lower.tail = FALSE)
   .t_crit <-
     qt(.alpha, df=(.n-1), lower.tail = FALSE)

   .sample_ci_low <- tdist_ci(.sample_mean, .sample_sd, .n,.t_crit,.return = "low")
   .sample_ci_hi <- tdist_ci(.sample_mean, .sample_sd, .n,.t_crit,.return = "hi")
  .ci_low <- exp(tdist_ci(.x,.sd,.n,.t_crit,.return="low"))
  .ci_hi <- exp(tdist_ci(.x,.sd,.n,.t_crit,.return="hi"))
  .geo_m <- exp(.x)

  return(
    data.frame(
      "sample_mean" = .sample_mean,
      "sample_median" = .median,
      "log_mean" = .x,
      "geom_mean" = .geo_m,
      "sample_stdev" = .sample_sd,
      "log_stdev" = .sd,
      "sample_size" = .n,
      "test_mean" = .raw_m,
      "t_val" = .t_val,
      "p_val" = .p_val,
      "t_crit" = .t_crit,
      "sample_ci_low" = .sample_ci_low,
      "sample_ci_hi" = .sample_ci_hi,
      "ci_low" = .ci_low,
      "ci_hi" = .ci_hi,
      "ci_method" = .ci_note,
      "method_note" = c("log_mean, ci_low, and ci_hi are based on log-transformed time data.")
    ))

}

#' @rdname time_vs_bench
#' @param .var If \code{.x} is a long-format data frame, the (unquoted) name of a data frame column containing task ratings (or values from another continuous metric).
#' @param ... (Optional) If \code{.x} is a long-format data frame, you can pass the name of one or more grouping variables as unquoted, comma-separated column names (without naming the \code{...} argument) to compute stats by groups.
#' @export
#'
time_vs_bench.data.frame<-
  function(.x, .var, .m,..., .alt = c("greater","less","twotailed"), .alpha=0.05){
  if (.alpha < 0 | .alpha > 1) {
    stop(".alpha must be a positive integer between 0 and 1")
  }
  if(missing(.m)){
    stop(
      "You need to specify .m as the test (benchmark) mean."
    )
  }

  .alt<-match.arg(.alt)

  if(.alt == "greater"){
    .ci_note <- paste0((1.0 -(2*.alpha))*100,"% CI using from a one-sided test based on the T distribution")
    .alpha <- (1.0 - .alpha)

     }
  else if(.alt == "less"){
    .ci_note <- paste0((1.0 -(2*.alpha))*100,"% CI from a one-sided test based on the T distribution")
    .alpha <- .alpha
    }
  else if(.alt == "twotailed"){
    .ci_note <- paste0((1.0 -(.alpha))*100,"% CI from a one-sided test based on the T distribution")
    .alpha <- (1.0 - (.alpha/2))
  }
  else {
    stop("Something went wrong. Did you provide the correct argument to .alt?")
  }

  .out <-
    dplyr::group_by(.x, ...)
  .out <-
    dplyr::summarise(
      .out,
      sample_mean = mean({{ .var }}, na.rm = TRUE),
      sample_median = stats::median({{ .var }}, na.rm = TRUE),
      log_mean = mean( log({{ .var }}), na.rm = TRUE),
      geom_mean = exp(mean( log({{ .var }}), na.rm = TRUE)),
      sample_stdev = stats::sd({{ .var }}, na.rm = TRUE),
      log_stdev = stats::sd(log({{ .var }}), na.rm = TRUE),
      sample_size = dplyr::n(),
      test_mean = as.numeric(.m),
      t_val = ((log(.m)-mean(log( {{ .var}}), na.rm=TRUE))/(stats::sd(log({{ .var }}), na.rm = TRUE)/sqrt(dplyr::n()))),
      p_val = stats::pt(((log(.m)-mean(log( {{ .var}}), na.rm=TRUE))/(stats::sd(log({{ .var }}), na.rm = TRUE)/sqrt(dplyr::n()))),df=(dplyr::n()-1), lower.tail = FALSE),
      t_crit = stats::qt(.alpha, df= (dplyr::n()-1), lower.tail = FALSE),
      .groups = "keep")

  .out$sample_ci_low <-""
  .out$sample_ci_hi <-""
  .out$ci_low <-""
  .out$ci_hi <-""
  .out$ci_method <- .ci_note
  .out$method_note <- c("log_mean, ci_low, and ci_hi are based on log-transformed time data.")

  .out<-
    dplyr::mutate(.out,
                  sample_ci_low = tdist_ci(.data$sample_mean, .data$sample_stdev,.data$sample_size,.data$t_crit,.return="low"),
                  sample_ci_hi = tdist_ci(.data$sample_mean, .data$sample_stdev,.data$sample_size,.data$t_crit,.return="hi"),
                  ci_low = exp(tdist_ci(.data$log_mean, .data$log_stdev,.data$sample_size,.data$t_crit,.return="low")),
                  ci_hi = exp(tdist_ci(.data$log_mean, .data$log_stdev,.data$sample_size,.data$t_crit,.return="hi"))
    )
.out
}
