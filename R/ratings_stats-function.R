#' Compute stats for ratings or continuously distributed data
#'
#' For ratings data and other continuously distributed variables, \code{ratings_stats()} returns means; information about confidence intervals (based on the T distribution); standard deviations; medians; and other details.
#'
#' @details
#' \itemize{
#'  \item You can modify the alpha level to adjust confidence intervals by including \code{.alpha} as a named argument and providing a numeric value: e.g., \code{.aplha = 0.001}.
#'  \item You can specific scale limits so that the output values have upper- and lower-bounds by including \code{.limits} and providing a numeric vector of length 2: e.g., \code{.limits = c(1.5,6.5)}.
#'  \item If you're passing a data frame to \code{.x}, you can optionally pass one or more grouping variables as unquoted, comma-separated column names (without naming the \code{...} argument) to compute stats by groups.
#' }
#'
#' Note that \code{NAs} are automatically dropped in all calculations.
#'
#'
#' @param .x A vector of values, or a long-format data frame with a named column containing numeric ratings data.
#' @param .var If \code{.x} is a data frame, the unquoted name of the data frame column containing the values to use in the computations.
#' @param ... (Optional) If \code{.x} is a long-format data frame, you can pass the name of one or more grouping variables as unquoted, comma-separated column names (without naming the \code{...} argument) to compute stats by groups.
#' @return A tibble with one or more means, confidence interval information, and other information.
#' @family descriptive stats for UX measures
#' @importFrom stats qt
#' @importFrom stats sd
#' @importFrom stats median
#' @importFrom dplyr n
#' @importFrom dplyr group_by
#' @importFrom dplyr summarise

#' @examples
#'
#' # Compare the difference between the output of:
#' ratings_stats(c(1,8,8)) # and:
#'
#' ratings_stats(c(1,8,8), .limits = c(1,8))
#'
#' .ux_data <-
#'  data.frame(
#'   "id" = rep(seq(1,10,1),2),
#'   "task" = c(rep(1,10),rep(2,10)),
#'   "easiness"  = sample(1:7,20,replace=TRUE))
#'
#' ratings_stats(.ux_data, easiness,task,.alpha=0.01,.limits=c(1,7))
#'
#' @rdname ratings_stats
#' @export
#'
#'
ratings_stats <- function(.x, ...) {
  UseMethod("ratings_stats", .x)
}


#' @rdname ratings_stats
#' @param .alpha (Optional) A positive number (where 0 < \code{.alpha} < 1) specifying the significance level to be used. Defaults to \code{.alpha = 0.05}. To set a different significance level, the argument must be named (i.e., \code{.alpha=0.001}) or else the function may yield unexpected results.
#' @param .limits (Optional) If you want to specify the end-points (limits) for the ratings scale, which will ensure that confidence interval values don't exceed the upper and lower bounds, you can supply a numeric vector of length two,indicating the limits (e.g., \code{.limits = c(1,7)}).

#'
#' @export
#'
ratings_stats.numeric<-function(.x,...,.alpha = 0.05,.limits=NULL){

if (.alpha < 0 | .alpha > 1) {
    stop(".alpha must be a positive integer between 0 and 1")
  }
  else {
    .p <- 1.0-(.alpha/2)
  }
  .m<-mean(.x, na.rm = TRUE)
  .sd<-stats::sd(.x, na.rm = TRUE)
  .n<-length(.x)
  .stderr <- (.sd/sqrt(.n))
  .tcrit <- stats::qt(p=.p, df=(.n-1))
  .me <- .tcrit * .stderr
  .median <- stats::median(.x, na.rm = TRUE)
  .out<-
    data.frame(
      "mean" = .m,
      "ci_low" = .m - .me,
      "ci_hi" = .m + .me,
      "ci_method" = paste0((1.0-.alpha)*100,"% CI for continuous data, based on T distrib."),
      "stdev" = .sd,
      "n" = .n,
      "t_crit" = .tcrit,
      "median" = .median
    )

  if(missing(.limits)){return(.out)}
  else if(length(.limits) != 2){
      stop(".limits should be a numeric vector of length 2; e.g., .limits=c(0,10)")
    }
  else{
      .limits<-as.numeric(.limits)
      .min <- min(.limits)
      .max <- max(.limits)

    .out$ci_low <- ifelse(.out$ci_low < .min,.min,.out$ci_low)
    .out$ci_hi <- ifelse(.out$ci_hi > .max,.max,.out$ci_hi)
    return(.out)
    }

  }



#' @rdname ratings_stats
#' @export
#'
ratings_stats.data.frame <- function(.x,
                               .var,
                               ...,
                               .alpha = 0.05,
                               .limits = NULL){

 if (.alpha < 0 | .alpha > 1) {
    stop(".alpha must be a positive integer between 0 and 1")
  }
  else {
    .p <- 1.0-(.alpha/2)
  }
  .out<-
    dplyr::group_by(.x, ...)
  .out<-
    dplyr::summarise(.out, mean = mean({{ .var }}, na.rm=TRUE),
              stdev = stats::sd({{ .var }}, na.rm = TRUE),
              median = stats::median({{ .var }}, na.rm = TRUE),
              n = dplyr::n())
  .out$stderr<- (.out$stdev/sqrt(.out$n))
  .out$tcrit <- stats::qt(p=.p, df=(.out$n-1))
  .out$me <- .out$tcrit * .out$stderr
  .out$ci_low <- .out$mean - .out$me
  .out$ci_hi <- .out$mean + .out$me
  .out$ci_method <- paste0((1.0-.alpha)*100,"% CI for continuous data, based on T distrib.")
  .out<-
    dplyr::ungroup(.out)

   if(missing(.limits)){return(.out)}
  else if(length(.limits) != 2){
    stop(".limits should be a numeric vector of length 2; e.g., .limits=c(0,10)")
  }
  else{
    .limits<-as.numeric(.limits)
    .min <- min(.limits)
    .max <- max(.limits)

    .out$ci_low <- ifelse(.out$ci_low < .min,.min,.out$ci_low)
    .out$ci_hi <- ifelse(.out$ci_hi > .max,.max,.out$ci_hi)
    return(.out)
  }

}
