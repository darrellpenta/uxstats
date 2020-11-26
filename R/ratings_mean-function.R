#' Compute a mean and confidence interval for continuously distributed data
#'
#' For ratings data and other continuously distributed variables, \code{ratings_mean()} returns a mean, along with information about the confidence interval (based on the T distribution). You can optionally include one or more grouping variables to compute means by groups; modify the alpha level to adjust confidence intervals; and specific scale limits so that the output values have upper- and lower-bounds.
#' @param .data  A vector or long-format data frame of ratings or other continuous data.
#' @param .var (Optional) The unquoted name of the data frame column containing the values to use in the computation.
#' @param ... (Optional) The unquoted, comma-separated names of columns containing grouping variables.
#' @return a tibble with one or more means
#' @family means with confidence intervals
#' @importFrom stats qt
#' @importFrom stats sd
#' @importFrom dplyr n
#' @importFrom dplyr filter
#' @importFrom dplyr group_by
#' @importFrom dplyr ungroup
#' @importFrom dplyr summarise
#' @importFrom dplyr mutate
#' @importFrom dplyr across
#' @importFrom dplyr relocate
#' @importFrom tidyselect any_of

#'
#' @rdname ratings_mean
#' @export
#'
#'
ratings_mean <- function(.data, ...) {
  UseMethod("ratings_mean", .data)
}


#' @rdname ratings_mean
#' @param .alpha (Optional) A positive number (where 0 < \code{.alpha} < 1) specifying the desired confidence level to be used. The argument must be named (i.e., \code{.alpha=0.001}) or else the function may yield unexpected results. If the argument is omitted, the default value is 0.05, or a 95\% confidence level.
#' @param .limits (Optional) If you want to specify the end-points (limits) for the ratings scale, which will ensure that confidence interval values don't exceed the upper and lwoer bounds, you can supply a numeric vector of length two,indicating the limits (e.g., \code{.limits = c(1,7)}).
#' @examples
#' # Compare the difference between:
#' ratings_mean(c(1,8,8))
#' # and:
#' ratings_mean(c(1,8,8), .limits = c(1,8))
#'
#' @export
#'
ratings_mean.numeric<-function(.data,...,.alpha = NULL,.limits=NULL){

  if(missing(.alpha)){
    .p<-0.975
  }
  else if (.alpha < 0 | .alpha > 1) {
    stop(".alpha must be a positive integer between 0 and 1")
  }
  else {
    .p <- 1.0-(.alpha/2)
  }
  .m<-mean(.data)
  .sd<-stats::sd(.data)
  .n<-length(.data)
  .serr = (.sd/sqrt(.n))
  .tcrit = stats::qt(p=.p, df=(.n-1))
  .me = .tcrit * .serr
  .out<-
    data.frame(
      "mean" = .m,
      "ci_low" = .m-.me,
      "ci_hi" = .m+.me,
      "stdev" = .sd,
      "n" = .n,
      "tcrit" = .tcrit
    )
  .out<-
    dplyr::mutate(.out, dplyr::across(tidyselect::any_of(c("ci_low","ci_hi","n","sd","serr","tcrit","me")), round,2))

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



#' @rdname ratings_mean
#' @export
#'
ratings_mean.data.frame <- function(.data,
                               .var,
                               ...,
                               .alpha = NULL,
                               .limits = NULL){

  if(missing(.alpha)){
    .p<-0.975
  }
  else if (.alpha < 0 | .alpha > 1) {
    stop(".alpha must be a positive integer between 0 and 1")
  }
  else {
    .p <- 1.0-(.alpha/2)
  }
  .out<-
    dplyr::group_by(.data, ...)
  .out<-
    dplyr::summarise(.out, mean = mean({{ .var }}),
              sd = stats::sd({{ .var }}),
              n = dplyr::n())
  .out$serr<- (.out$sd/sqrt(.out$n))
  .out$tcrit <- stats::qt(p=.p, df=(.out$n-1))
  .out$me <- .out$tcrit * .out$serr
  .out$ci_low <- .out$mean - .out$me
  .out$ci_hi <- .out$mean + .out$me
  .out<-
    dplyr::ungroup(.out)
  .out<-
    dplyr::relocate(.out,
                    tidyselect::any_of(c("ci_low","ci_hi","n")), .after=mean)

  .out<-
    dplyr::mutate(.out, dplyr::across(tidyselect::any_of(c("ci_low","ci_hi","n","sd","serr","tcrit","me")), round,2))
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
