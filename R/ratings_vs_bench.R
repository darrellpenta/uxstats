#' Compare ratings to a benchmark
#
#' @param .x  The sample mean.
#' @param .m  The benchmark mean.
#' @param .sd  The sample standard deviation.
#' @param .n  The sample size
#' @param ... (Optional) The unquoted, comma-separated names of columns containing grouping variables.
#' @param .alpha (Optional) A positive number (where 0 < \code{.alpha} < 1) specifying the desired confidence level to be used. The argument must be named (i.e., \code{.alpha=0.001}) or else the function may yield unexpected results. If the argument is omitted, the default value is 0.05, corresponding to a 90\% confidence level for a one-sided test.
#' @return A tibble with test results
#' @family means with confidence intervals
#' @importFrom stats qnorm
#' @importFrom stats pnorm
#' @importFrom stats pt
#' @importFrom dplyr n
#' @importFrom dplyr bind_rows
#' @importFrom dplyr group_by
#' @importFrom dplyr group_modify
#' @importFrom dplyr summarise
#' @importFrom rlang .data
#' @include adjwald_ci-function.R
#' @include laplace-function.R
#' @examples
#'
#' .uxdata <-
#' data.frame(
#'  "id" = rep(seq(1,10,1),2),
#'  "group" = rep(c("A","B"),10),
#'  "task" = c(rep(1,10),rep(2,10)),
#'  "ratings"  = runif(20,0,100)
#' )
#'
#' ratings_vs_bench(.uxdata, .m=75, .var=ratings,group)
#' @rdname ratings_vs_bench
#' @export
#'
ratings_vs_bench <- function(.x, ...) {
  UseMethod("ratings_vs_bench", .x)
}

#' @rdname ratings_vs_bench
#' @export

ratings_vs_bench.numeric<-function(.x,.m,.sd,.n,...,.alpha=0.10){

.t_val <-
  (.x-.m)/(.sd/sqrt(.n))
.prob<-
  pt(q = abs(.t_val), df=(.n-1), lower.tail = FALSE)
.t_crit <-
  qt(.alpha, df=(.n-1), lower.tail = FALSE)

.ci_low <- .x-(.t_crit*(.sd/sqrt(.n)))
.ci_hi <- .x+(.t_crit*(.sd/sqrt(.n)))
return(
  data.frame(
    "sample mean" = .x,
    "sample stdev" = .sd,
    "test mean" = .m,
    "sample size" = .n,
    "pval" = .prob,
    "critical_t" = .t_crit,
    "ci_low" = .ci_low,
    "ci_hi" = .ci_hi
  ))
}

#' @rdname ratings_vs_bench
#' @param .var  The benchmark mean.
#' @export
#'
ratings_vs_bench.data.frame<-function(.x,.m,.var,...,.alpha=0.10){
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
                                                   .m = .m,
                                                   .sd = .x$stdev,
                                                   .n = .x$n,
                                                   .alpha = .alpha),
                        .keep = TRUE)

  return(.out)

}
