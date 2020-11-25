#' Returns a mean with confidence intervals based on the t-distribution.
#'
#' @param .data  A vector or data frame of values (ratings)
#' @param .var (Optional) The unquoted name of the data frame column containing the values to use in the computation.
#' @param ... (Optional) The unquoted, comma-separated names of columns containing grouping variables.
#' @param .alpha (Optional) A numeric value specifying the desired confidence level to be used. If omitted, defaults to 0.05.
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
#' @importFrom dplyr relocate
#' @importFrom tidyselect any_of

#'
#' @rdname ux_mean
#' @export
#'
#'
ux_mean <- function(.data, ...) {
  UseMethod("ux_mean", .data)
}

#' @rdname ux_mean
#' @export
#'
ux_mean.default<-function(.data,...){
  NextMethod("ux_mean")
}

#' @rdname ux_mean
#' @param .alpha (Optional) A positive number (where 0 < \code{.alpha} <1) specifying the desired confidence level to be used. The argument must be named (i.e., \code{.alpha=0.001}) or else the function may yield unexpected results. If the argument is omitted, it defaults to 0.05.
#' @export
#'
ux_mean.numeric<-function(.data,...,.alpha = NULL){

  if(missing(.alpha)){
    .p<-0.95
  }
  else if (.alpha < 0 | .alpha > 1) {
    stop(".alpha must be a positive integer between 0 and 1")
  }
  else {
    .p <- 1.0-.alpha
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
      "ci_lo" = .m-.me,
      "ci_hi" = .m+.me,
      "stdev" = .sd,
      "n" = .n,
      "tcrit" = .tcrit
    )
  .out
}


#' @rdname ux_mean
#' @export
#'
ux_mean.data.frame <- function(.data,
                               .var,
                               ...,
                               .alpha = NULL){
  # .var <- enquo(.var)
  # .group_vars <- enquos(...)
  if(missing(.alpha)){
    .p<-0.95
  }
  else if (.alpha < 0 | .alpha > 1) {
    stop(".alpha must be a positive integer between 0 and 1")
  }
  else {
    .p <- 1.0-.alpha
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
  .out$ci_lo <- .out$mean - .out$me
  .out$ci_hi <- .out$mean + .out$me
  .out<-
    dplyr::ungroup(.out)
  .out<-
    dplyr::relocate(.out,
                    tidyselect::any_of(c("ci_lo","ci_hi","n")), .after=mean)

  return(.out)

}
