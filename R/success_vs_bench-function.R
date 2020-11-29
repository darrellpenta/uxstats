#' Compare a success rate to a benchmark
#
#' @param .x  The number of observed successes.
#' @param .n  The total number of users.
#' @param .p  The test proportion (must be a numeric between 0-1).
#' @param ... (Optional) The unquoted, comma-separated names of columns containing grouping variables.
#' @param .alt For alternatives, one of \code{c("greater","less","twotailed")}. Defaults to "greater" for a one-sided test.
#' @param .alpha (Optional) A positive number (where 0 < \code{.alpha} < 1) specifying the desired confidence level to be used. The argument must be named (i.e., \code{.alpha=0.001}) or else the function may yield unexpected results. If the argument is omitted, the default value is 0.05, corresponding to a 90\% confidence level for a one-sided test.
#' @return A tibble with test results
#' @family means with confidence intervals
#' @importFrom stats qnorm
#' @importFrom dplyr n
#' @importFrom dplyr bind_rows
#' @importFrom dplyr group_by
#' @importFrom dplyr group_modify
#' @importFrom dplyr summarise
#' @importFrom rlang .data
#' @include adjwald_ci-function.R

#' @rdname success_vs_bench
#' @export
#'
success_vs_bench <- function(.x, ...) {
  UseMethod("success_vs_bench", .x)
}

#' @rdname success_vs_bench
#' @export

success_vs_bench.numeric<-function(.x,.n,.p, ...,.alt=c("greater","less","twotailed"),.alpha=0.05){

 if (.alpha < 0 | .alpha > 1) {
    stop(".alpha must be a positive integer between 0 and 1")
 }
  .alt<-match.arg(.alt)
  if(.alt == "greater"){
     .Z <- stats::qnorm((1.0 -.alpha))
  }
  else if(.alt == "less"){
     .Z <- stats::qnorm(.alpha)
  }
  else if(.alt == "twotailed"){
  .Z <- stats::qnorm(1.0 - (.alpha/2))
  }
  else {
    stop("Something went wrong. Did you provide the correct argument to .alt?")
  }

# if(.x <= 15 & (.n-.x) <= 15){
    .success <- c(.x, .x+seq(1,(.n-.x),1))
    .users <- c(rep(.n,times=length(c(.x, .x+seq(1,(.n-.x),1)))))
     .new<-
      data.frame(
        "success" = .success,
        "users"   = .users
      )

   .new<-
    dplyr::bind_rows(
      apply(.new,1,
          function(.xx,.p_=.p,.alt_=.alt, .alpha_=.alpha){
            .num<-factorial(.xx[2])
            .denom<-factorial(.xx[1]) * factorial(.xx[2]-.xx[1])
            .pval_exact<-(.num/.denom)*(.p_^.xx[1])*((1.0-.p_)^(.xx[2]-.xx[1]))

            return(
              tibble::tibble(
                "pval_exact" = .pval_exact)
              )
            })
      )

.new$adjusted_pval <- .new$pval_exact
.new$adjusted_pval[1] <- (.5 * .new$pval_exact[1])
.new<-
  dplyr::summarise(.new,
                   `pval_exact` = sum(.data$pval_exact),
                   `pval_mid`=sum(.data$adjusted_pval),
                   .groups="keep")

.new$successes <-.x
.new$users <- .n
.new$observed_success <- (.x/.n)*100
.new$success_exact_prob <- (100-(.new$pval_exact*100))
.new$success_mid_prob <- (100-(.new$pval_mid*100))
.new$adjwald_ci_low <- adjwald_ci(.x,.n,.Z=.Z)[[1]]*100
.new$adjwald_ci_hi <- adjwald_ci(.x,.n,.Z=.Z)[[2]]*100

return(.new[,c(3,4,5,1,2,6,7,8,9)])
  # } else{
  # return("wah wah") }

}

#' @examples
#'
#' .uxdata <-
#' data.frame(
#'  "id" = rep(seq(1,10,1),2),
#'  "group" = rep(c("A","B"),10),
#'  "task" = c(rep(1,10),rep(2,10)),
#'  "complete"  = sample(0:1,20,replace=TRUE,prob = c(.3,.65))
#' )
#'
#' success_vs_bench(.uxdata, complete, .p=0.7,group)
#' @rdname success_vs_bench
#' @export
success_vs_bench.data.frame<-function(.x,.n,.p, ...,.alt=c("greater","less","twotailed"),.alpha=0.05){

  # if (.alpha < 0 | .alpha > 1) {
  #   stop(".alpha must be a positive integer between 0 and 1")
  # }
  # .alt<-match.arg(.alt)
  # if(.alt == "greater"){
  #   .Z <- stats::qnorm((1.0 -.alpha))
  # }
  # else if(.alt == "less"){
  #   .Z <- stats::qnorm(.alpha)
  # }
  # else if(.alt == "twotailed"){
  #   .Z <- stats::qnorm(1.0 - (.alpha/2))
  # }
  # else {
  #   stop("Something went wrong. Did you provide the correct argument to .alt?")
  # }
  #

  .out <-
    dplyr::group_by(.x, ...)
  .out <-
    dplyr::summarise(
      .out,
      successes = sum({{ .n }}),
      users = dplyr::n(),
      .groups = "keep")

  .out <-
    dplyr::group_modify(.out,
                        ~ success_vs_bench.numeric(.x=.x$successes,
                                                .n = .x$users,
                                                .p = .p,
                                                .alt = .alt,
                                                .alpha = .alpha),
                        .keep = TRUE)

  return(.out)

}
