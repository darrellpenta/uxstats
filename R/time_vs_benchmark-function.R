#' Compare sample time to a benchmark
#
#' @param .x  The number of observed successes.
#' @param .m  The benchmark mean.
#' @param .sd  The sample standard deviation.
#' @param .n  The total number of users.
#' @param ... (Optional) The unquoted, comma-separated names of columns containing grouping variables.
#' @param .alpha (Optional) A positive number (where 0 < \code{.alpha} < 1) specifying the desired confidence level to be used. The argument must be named (i.e., \code{.alpha=0.001}) or else the function may yield unexpected results. If the argument is omitted, the default value is 0.05, corresponding to a 90\% confidence level for a one-sided test.
#' @return A tibble with test results
#' @family benchmark comparison stats
#' @importFrom stats qnorm
#' @importFrom stats pnorm
#' @importFrom dplyr n
#' @importFrom dplyr bind_rows
#' @importFrom dplyr group_by
#' @importFrom dplyr group_modify
#' @importFrom dplyr summarise
#' @importFrom rlang .data
#' @include adjwald_ci-function.R
#' @include laplace-function.R

#' @rdname time_vs_bench
#' @export
#'
time_vs_bench <- function(.x, ...) {
  UseMethod("time_vs_bench", .x)
}

#' @rdname time_vs_bench
#' @export

time_vs_bench.numeric <- function(.x,.m,.sd,.n,...,.alpha=0.10){
  .t_val_num <-
    (.m-.x)
   .t_val_denom <-(.sd/sqrt(.n))
  .t_val<-.t_val_num/.t_val_denom
   .prob<-
    pt(q = .t_val, df=(.n-1), lower.tail = FALSE)
   .t_crit <-
     qt(.alpha, df=(.n-1), lower.tail = FALSE)

   .ci_low <- .x-(.t_crit*(.sd/sqrt(.n)))
   .ci_hi <- .x+(.t_crit*(.sd/sqrt(.n)))
  .ci_low <- exp(.x-(.t_crit*(.sd/sqrt(.n))))
  .ci_hi <- exp(.x+(.t_crit*(.sd/sqrt(.n))))
  .geo_m <- exp(.x)

  return(
    data.frame(
      "sample_mean" = .x,
      "sample_stdev" = .sd,
      "test_mean" = .m,
      "sample_size" = .n,
      "geom_mean" = .geo_m,
      "pval" = .prob,
      "critical_t" = .t_crit,
      "ci_low" = .ci_low,
      "ci_hi" = .ci_hi
    ))

}

#' @rdname time_vs_bench
#' @param .var  The (unquoted) name of the column containing the time data.
#' .uxdata <-
#' data.frame(
#'  "id" = rep(seq(1,10,1),2),
#'  "group" = rep(c("A","B"),10),
#'  "task" = c(rep(1,10),rep(2,10)),
#'  "time"  = runif(20,0,1000)
#' )
#'
#' time_stats(.uxdata, .var=time, group, task, .alpha = 0.001)

#' @export
#'
time_vs_bench.data.frame<-function(.x,.m,.var,...,.alpha=0.10){
  .out <-
    dplyr::group_by(.x, ...)
  .out <-
    dplyr::summarise(
      .out,
      mean = mean( log({{ .var }}), na.rm = TRUE),
      stdev = stats::sd(log({{ .var }}), na.rm = TRUE),
      n = dplyr::n(),
      .groups = "keep")

  .out <-
    dplyr::group_modify(.out,
                        ~ time_vs_bench.numeric(.x=.x$mean,
                                                   .m = log(.m),
                                                   .sd = .x$stdev,
                                                   .n = .x$n,
                                                   .alpha = .alpha),
                        .keep = TRUE)

  return(.out)

}
