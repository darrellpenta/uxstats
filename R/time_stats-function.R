#' Compute stats for time data
#'
#' \href{https://g.co/kgs/a7Zyyn}{Sauro and Lewis (2012)} describe various approaches for computing stats on task time or response time data when you're working with smaller sample sizes. \code{time_stats()} returns several point estimates, including means, geometric means (better when sample size <= 25), and medians (better for larger sample sizes). It also returns confidence interval information based on transformed data (for small samples) or raw data (for larger samples) for the sample size, andincludes a field to indicate whether the estimates are baed on raw or transformed data. You can optionally include one or more grouping variables to compute statistics by groups, and modify the alpha level to adjust confidence intervals.
#' `success_rate()` and `completion_rate()` are synonyms.
#
#' @param .data  A vector or long-format data frame of numeric data corresponding to task or response times.
#' @param .var (Optional) The unquoted name of the data frame column containing the values to use in the computation.
#' @param ... (Optional) The unquoted, comma-separated names of columns containing grouping variables.
#' @param .alpha (Optional) A positive number (where 0 < \code{.alpha} < 1) specifying the desired confidence level to be used. The argument must be named (i.e., \code{.alpha=0.001}) or else the function may yield unexpected results. If the argument is omitted, the default value is 0.05, or a 95\% confidence level.
#' @return A tibble with one or more means, confidence interval information, and other information.
#' @family means with confidence intervals
#' @importFrom stats qt
#' @importFrom stats sd
#' @importFrom stats median
#' @importFrom dplyr n
#' @importFrom dplyr filter
#' @importFrom dplyr group_by
#' @importFrom dplyr ungroup
#' @importFrom dplyr summarise
#' @importFrom dplyr across
#' @importFrom dplyr group_modify
#' @examples
#' time_stats(c(40, 36, 53, 56, 110, 48, 34, 44, 30, 40, 80))
#'
#' .uxdata <-
#' data.frame(
#'  "id" = rep(seq(1,10,1),2),
#'  "group" = rep(c("A","B"),10),
#'  "task" = c(rep(1,10),rep(2,10)),
#'  "time"  = runif(20,0,1000)
#' )
#'
#' time_stats(.uxdata, .var=time, group, task, .alpha = 0.001)
#' @rdname time_stats
#' @export
#'
#'
time_stats <- function(.data, ...) {
  UseMethod("time_stats", .data)
}


#' @rdname time_stats
#' @export
#'
time_stats.numeric<-function(.data,...,.alpha = NULL){

  if(missing(.alpha)){
    .p<-0.975
  }
  else if (.alpha < 0 | .alpha > 1) {
    stop(".alpha must be a positive integer between 0 and 1")
  }
  else {
    .p <- 1.0-(.alpha/2)
  }
  if(length(.data) <= 25){
    .m_raw <- mean(.data, na.rm = TRUE)
    .m_trans <-mean(log(.data), na.rm = TRUE)
    .geo_m <- exp(mean(log(.data),na.rm = TRUE))
    .median <- stats::median(.data, na.rm = TRUE)
    .sd_raw <- stats::sd(.data, na.rm = TRUE)
    .sd_trans <- stats::sd(log(.data), na.rm = TRUE)
    .n <- length(.data)
    .stderr_raw <- (.sd_raw/sqrt(.n))
    .stderr_trans <- (.sd_trans/sqrt(.n))
    .tcrit <- stats::qt(p=.p, df=(.n-1))
    .me_raw <- .tcrit * .stderr_raw
    .me_trans <- .tcrit * .stderr_trans
    .raw_trans <- "transformed (small n)"

    .out<-
      data.frame(
        "mean" = .m_raw,
        "median" = .median,
        "geom_mean" = .geo_m,
        "ci_low" = exp(.m_trans -.me_trans),
        "ci_hi" = exp(.m_trans +.me_trans),
         "stdev" = .sd_raw,
        "n" = .n,
        "t_z_crit" = .tcrit,
        "raw_transform" = .raw_trans
      )
    .out
  }
  else{
    .m <- mean(.data, na.rm = TRUE)
    .stdev <- stats::sd(.data, na.rm = TRUE)
    .geo_m <- exp(mean(log(.data),na.rm = TRUE))
    .median <- stats::median(.data, na.rm = TRUE)
    .n <- length(.data)
    .zcrit <- stats::qnorm(p=.p)
    .stderr <-sqrt((.n*0.5) * 0.5)
    .me <- .zcrit * .stderr
    .ceil_low <- ceiling((.n * .5) - .me)
    .ceil_hi <- ceiling((.n * .5) + .me)
    .data <- sort(.data)
    .ci_low <- .data[.ceil_low]
    .ci_hi <- .data[.ceil_hi]
    .raw_trans <- "raw (large n)"

.out<-
      data.frame(
        "mean" = .m,
        "geom_mean" = .geo_m,
        "median" = .median,
        "ci_low" = .ci_low,
        "ci_hi" = .ci_hi,
        "n" = .n,
        "t_z_crit" = .zcrit,
        "stdev" = .stdev,
        "raw_transform" = .raw_trans
      )
  }
    return(.out)
  }



#' @rdname time_stats
#' @export
#'
time_stats.data.frame <- function(.data,
                               .var,
                               ...,
                               .alpha = NULL){

  if(missing(.alpha)){
    .p<-0.975
    .alpha2 <- 0.05
  }
  else if (.alpha < 0 | .alpha > 1) {
    stop(".alpha must be a positive integer between 0 and 1")
  }
  else {
    .p <- 1.0-(.alpha/2)
    .alpha2 <- .alpha
  }

  .data<-
    dplyr::select(.data,`new_index` = {{ .var}}, tidyselect::everything())
  .data<-
    dplyr::group_by(.data, ...)
  .out <-
    dplyr::group_modify(.data,
                        ~ time_stats.numeric(.x$new_index, .alpha=.alpha2),
                     .keep = TRUE)

return(.out)
}
