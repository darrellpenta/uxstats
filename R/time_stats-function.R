#' Compute stats for time data
#'
#' Following \href{https://g.co/kgs/a7Zyyn}{Sauro and Lewis (2012)}, \code{time_stats()} takes the sample size into account when computing stats on time data.
#'
#' @details
#' \code{time_stats()} returns several point estimates, including means, geometric means (better when sample size <= 25), and medians (better for larger sample sizes). It also returns confidence interval information based on log-transformed values (for small samples) or raw (untransformed) data (for larger samples).
#'  \itemize{
#'  \item You can modify the alpha level to adjust confidence intervals by including \code{.alpha} as a named argument and providing a numeric value: e.g., \code{.aplha = 0.001}.
#'  \item If you're passing a data frame to \code{.x}, you can optionally pass one or more grouping variables as unquoted, comma-separated column names (without naming the \code{...} argument) to compute stats by groups.
#'   }
#'
#' Note that \code{NAs} are automatically dropped in all calculations.
#'
#' @param .x  A vector or long-format data frame with a named column of numeric values corresponding to task or response times.
#' @param .var If \code{x} is a data frame, the unquoted name of column containing the values to use in the computations.
#' @param ... (Optional) If \code{.x} is a long-format data frame, you can pass the name of one or more grouping variables as unquoted, comma-separated column names (without naming the \code{...} argument) to compute stats by groups.
#' @param .alpha (Optional) A positive number (where 0 < \code{.alpha} < 1) specifying the significance level to be used. Defaults to \code{.alpha = 0.05}. To set a different significance level, the argument must be named (i.e., \code{.alpha=0.001}) or else the function may yield unexpected results.
#' @return A tibble with one or more means, confidence interval information, and other information.
#' @family descriptive stats for UX measures
#' @importFrom stats qt sd median
#' @importFrom dplyr n group_by summarise group_modify
#' @include tinv-function.R
#' @include tdist_ci-function.R
#' @examples
#' time_stats(c(40, 36, 53, 56, 110, 48, 34, 44, 30, 40, 80))
#'
#' .ux_data <-
#' data.frame(
#'  "id" = rep(seq(1,10,1),2),
#'  "group" = rep(c("A","B"),10),
#'  "task" = c(rep(1,10),rep(2,10)),
#'  "time"  = runif(20,200,1000)
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
    .tcrit <- tinv(.alpha, .df=(.n-1))
    .me_raw <- .tcrit * .stderr_raw
    .me_trans <- .tcrit * .stderr_trans
    .raw_trans <- "transformed (small n)"

    .out<-
      data.frame(
        "mean" = .m_raw,
        "median" = .median,
        "geom_mean" = .geo_m,
        "ci_low" = exp(tdist_ci(.m_trans, .sd_trans,.n,.tcrit,.return = "low")),
        "ci_hi" = exp(tdist_ci(.m_trans, .sd_trans,.n,.tcrit,.return = "hi")),
        "ci_method" = paste0((1.0-.alpha)*100,"% CI using the exponent of the log-transformed data, based on the T distribution."),
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
    .zcrit <- stats::qnorm(1.0-(.alpha/2))
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
        "ci_low" = .ci_low,
        "ci_hi" = .ci_hi,
        "ci_method" = paste0((1.0-.alpha)*100,"% CI around the median using the binomial distribution method in Suaro and Lewis (2012)."),
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

  .x<-
    dplyr::select(.x,`new_index` = {{ .var}}, tidyselect::everything())
  .x<-
    dplyr::group_by(.x, ...)
  .out <-
    dplyr::group_modify(.x,
                        ~ time_stats.numeric(.x$new_index, .alpha=.alpha),
                     .keep = TRUE)

return(.out)
}
