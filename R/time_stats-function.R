#' Compute stats for time data
#'
#' Following \href{https://g.co/kgs/a7Zyyn}{Sauro and Lewis (2012)}, \code{time_stats()} takes the sample size into account when computing stats on time data.
#'
#' @details
#' \code{time_stats()} returns several point estimates, including means, geometric means (better when sample size <= 25), and medians (better for larger sample sizes). It also returns confidence interval information based on log-transformed values (for small samples) or raw (untransformed) data (for larger samples).
#' * You can modify the alpha level to adjust confidence intervals by including \code{.alpha} as a named argument and providing a numeric value: e.g., \code{.aplha = 0.001}.
#' * If you're passing a data frame to \code{.x}, you can optionally include one or more grouping variables to compute stats by groups.
#'
#' Note that \code{NAs} are automatically dropped in all calculations.
#'
#' @param .x  A vector or long-format data frame with a named column of numeric values corresponding to task or response times.
#' @param .var If \code{x} is a data frame, the unquoted name of column containing the values to use in the computations.
#' @param ... (Optional) If \code{x} is a data frame, the unquoted, comma-separated names of columns containing grouping variables.
#' @param .alpha (Optional) A positive number (where 0 < \code{.alpha} < 1) specifying the desired confidence level to be used. The argument must be named (i.e., \code{.alpha=0.001}) or else the function may yield unexpected results. If the argument is omitted, the default value is 0.05, or a 95\% confidence level.
#' @return A tibble with one or more means, confidence interval information, and other information.
#' @family descriptive stats for UX measures
#' @importFrom stats qt
#' @importFrom stats sd
#' @importFrom stats median
#' @importFrom dplyr n
#' @importFrom dplyr group_by
#' @importFrom dplyr ungroup
#' @importFrom dplyr summarise
#' @importFrom dplyr group_modify
#' @examples
#' time_stats(c(40, 36, 53, 56, 110, 48, 34, 44, 30, 40, 80))
#'
#' .ux_data <-
#' data.frame(
#'  "id" = rep(seq(1,10,1),2),
#'  "group" = rep(c("A","B"),10),
#'  "task" = c(rep(1,10),rep(2,10)),
#'  "time"  = runif(20,0,1000)
#' )
#'
#' time_stats(.ux_data, .var=time, task, .alpha = 0.001)
#' @rdname time_stats
#' @export
#'
#'
time_stats <- function(.x, ...) {
  UseMethod("time_stats", .x)
}


#' @rdname time_stats
#' @export
#'
time_stats.numeric<-function(.x,...,.alpha = 0.05){

   if (.alpha < 0 | .alpha > 1) {
    stop(".alpha must be a positive integer between 0 and 1")
  }
  else {
    .p <- 1.0-(.alpha/2)
  }
  if(length(.x) <= 25){
    .m_raw <- mean(.x, na.rm = TRUE)
    .m_trans <-mean(log(.x), na.rm = TRUE)
    .geo_m <- exp(mean(log(.x),na.rm = TRUE))
    .median <- stats::median(.x, na.rm = TRUE)
    .sd_raw <- stats::sd(.x, na.rm = TRUE)
    .sd_trans <- stats::sd(log(.x), na.rm = TRUE)
    .n <- length(.x)
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
        "ci_method" = paste0((1-.alpha)*100,"% CI using log-transformed data, based on the T distribution."),
        "ci_low" = exp(.m_trans -.me_trans),
        "ci_hi" = exp(.m_trans +.me_trans),
         "stdev" = .sd_raw,
        "n" = .n,
        "t_z_crit" = .tcrit,
        "method note" = .raw_trans
      )
    .out
  }
  else{
    .m <- mean(.x, na.rm = TRUE)
    .stdev <- stats::sd(.x, na.rm = TRUE)
    .geo_m <- exp(mean(log(.x),na.rm = TRUE))
    .median <- stats::median(.x, na.rm = TRUE)
    .n <- length(.x)
    .zcrit <- stats::qnorm(p=.p)
    .stderr <-sqrt((.n*0.5) * 0.5)
    .me <- .zcrit * .stderr
    .ceil_low <- ceiling((.n * .5) - .me)
    .ceil_hi <- ceiling((.n * .5) + .me)
    .x <- sort(.x)
    .ci_low <- .x[.ceil_low]
    .ci_hi <- .x[.ceil_hi]
    .raw_trans <- "raw (large n)"

.out<-
      data.frame(
        "mean" = .m,
        "geom_mean" = .geo_m,
        "median" = .median,
        "ci_method" = paste0((1-.alpha)*100,"% CI around the median using the binomial distribution method in Suaro and Lewis (2012)."),
        "ci_low" = .ci_low,
        "ci_hi" = .ci_hi,
        "n" = .n,
        "t_z_crit" = .zcrit,
        "stdev" = .stdev,
        "method note" = .raw_trans
      )
  }
    return(.out)
  }



#' @rdname time_stats
#' @export
#'
time_stats.data.frame <- function(.x,
                               .var,
                               ...,
                               .alpha = 0.05){


if (.alpha < 0 | .alpha > 1) {
    stop(".alpha must be a positive integer between 0 and 1")
  }
  else {
    .p <- 1.0-(.alpha/2)
    .alpha2 <- .alpha
  }

  .x<-
    dplyr::select(.x,`new_index` = {{ .var}}, tidyselect::everything())
  .x<-
    dplyr::group_by(.x, ...)
  .out <-
    dplyr::group_modify(.x,
                        ~ time_stats.numeric(.x$new_index, .alpha=.alpha2),
                     .keep = TRUE)

return(.out)
}
