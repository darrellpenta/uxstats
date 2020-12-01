#' Compare a success rate to a benchmark
#'
#' @description
#' \code{success_vs_bench()} tests an observed success rate against a given benchmark. Following \href{https://g.co/kgs/a7Zyyn}{Sauro and Lewis (2012)}, it takes the sample size into account in providing estimates.
#'
#' \code{success_vs_bench()} and \code{completion_vs_bench()} are synonyms.
#'
#' @details
#' \code{success_vs_bench()} returns a variety of estimates. \href{https://g.co/kgs/a7Zyyn}{Sauro and Lewis (2012)} recommend using the mid-probability from the binomial distribution for small sample sizes (i.e., cases with fewer than 15 successes and 15 failures), and for large sample sizes, using the normal approximation to the binomial. The function also reports the best estimate success rate using the Laplace calculation.
#'
#' \code{success_vs_bench} assumes that you want to test the hypothesis that the observed outcome \emph{exceeds} the benchmark, and therefore, defaults to a one-tailed test. This means that setting \code{.alpha = 0.05} (the default) produces a 90\% confidence interval.
#'
#' \itemize{
#'   \item If \code{.x} is a single numeric value representing the total number of successes, \code{.n} should be a single numeric value representing the total number of users, where the value of \code{.n} >= the value of \code{.x}). e.g., \code{.x = 23, .n = 25}
#'   \item If \code{.x} is a data frame, \code{.var} should be the unquoted name of the column containing the success data (as 1s and 0s).
#'   \item If you're passing a data frame to \code{.x}, you can optionally pass one or more grouping variables as unquoted, comma-separated column names (without naming the \code{...} argument) to compute stats by groups.
#'   \item You can choose from among the test alternatives \code{c("greater","less","twotailed")} by providing one of the options to the \code{.alt} argument: e.g., \code{.alt = "twotailed"}. Defaults to "greater" for a one-sided test.
#'   \item You can modify the alpha level to adjust confidence intervals by including \code{.alpha} as a named argument and providing a numeric value: e.g., \code{.aplha = 0.001}.
#' }
#'
#' Note that \code{NAs} are automatically dropped in all calculations.
#'
#'
#' @param .x A single numeric value, a vector of values, or a long-format data frame with a named column of numeric data (1s and/or 0s) corresponding to task success outcomes. See Details.
#' @param .n  A single numeric value representing the total number of trials. See Details.
#' @param .p  The test (benchmark) proportion (must be a numeric between 0-1).
#' @param .alt For test alternatives, one of \code{c("greater","less","twotailed")}. Defaults to "greater" for a one-sided test.
#' @param .alpha (Optional) A positive number (where 0 < \code{.alpha} < 1) specifying the significance level to be used. Defaults to \code{.alpha = 0.05}. To set a different significance level, the argument must be named (i.e., \code{.alpha=0.001}) or else the function may yield unexpected results.
#' @return A tibble with data summaries and test results
#' @family benchmark comparison stats
#' @importFrom stats qnorm pnorm
#' @importFrom dplyr n bind_rows group_by group_modify summarise
#' @importFrom rlang .data
#' @importFrom tibble tibble
#' @include adjwald_ci-function.R
#' @include laplace-function.R

#' @rdname success_vs_bench
#' @export
#'
success_vs_bench <- function(.x, ...) {
  UseMethod("success_vs_bench", .x)
}
#' @rdname success_vs_bench
#' @export
completion_vs_bench <- success_vs_bench
#'
#'

#' @rdname success_vs_bench
#' @export

success_vs_bench.numeric<-function(.x,.n = NULL,.p,...,.alt=c("greater","less","twotailed"),.alpha=0.05){
 if (.alpha < 0 | .alpha > 1) {
    stop(".alpha must be a positive number between 0 and 1")
 }
 if(missing(.p)){
   stop("You need to specify .p as the test (benchmark) proportion")
 } else if (.p < 0 | .p > 1) {
    stop(".p must be a positive number between 0 and 1")
 }
  if(length(.x)==1 && missing(.n)) {
    stop(
      "You need to specify .n as the total number of users (i.e., the sample size)."
    )
  }
.alt<-match.arg(.alt)
  if(.alt == "greater"){
    .Z <- stats::qnorm((1.0 -.alpha))
    .ci_note <- paste0((1.0 -(2*.alpha))*100,"% Adjusted Wald CI from a one-sided test")
  }
  else if(.alt == "less"){
    .Z <- stats::qnorm(.alpha)
    .ci_note <- paste0((1.0 -(2*.alpha))*100,"% Adjusted Wald CI from a one-sided test")
  }
  else if(.alt == "twotailed"){
    .Z <- stats::qnorm(1.0 - (.alpha/2))
    .ci_note <- paste0((1.0 -(.alpha))*100,"% Adjusted Wald CI from a two-sided test")
  }
  else {
    stop("Something went wrong. Did you provide the correct argument to .alt?")
  }

  if(length(.x) > 1){
    if (any(.x >1,na.rm = TRUE)){
      stop("If you're passing a vector of values, the vector should contain only 1s (for successes) and 0s (for failures).")
    }
    else{
    .n <-
      length(.x[!is.na(.x)])
    .x <-
      sum(.x, na.rm=TRUE)
    }
  }
if(.x > .n) {
  stop(
    "The value of .x is larger than the value of .n. Check your data."
  )
}
if(.x >= 15 && (.n-.x) >= 15){
  .out<-
    data.frame(
      "successes"  = .x,
      "users" = .n,
      "observed_success_pct" = (.x/.n)*100,
      "pval_exact" = (1 - stats::pnorm(  ((.x/.n) - .p)/sqrt((.p * (1-.p))/.n),lower.tail = TRUE)),
      "pval_mid" = NA_integer_,
      "success_exact_prob" = 100 * stats::pnorm(  ((.x/.n) - .p)/sqrt((.p * (1-.p))/.n),lower.tail = TRUE),
      "success_mid_prob" = NA_integer_,
      "success_best_estimate" = laplace(.x,.n) * 100,
      "success_estimator" = c("Laplace"),
      "ci_low" = adjwald_ci(.x,.n,.Z=.Z)[[1]]*100,
      "ci_hi" = adjwald_ci(.x,.n,.Z=.Z)[[2]]*100,
      "ci_method" = .ci_note,
      "method_note" = c("large sample"))
  .out
  }else{

  .out <-
    success_vs_bench_smallsample(.x=.x,.n=.n,.p=.p)
 .out$success_best_estimate <- laplace(.x,.n) * 100
 .out$success_estimator <- c("Laplace")
 .out$ci_low <- adjwald_ci(.x,.n,.Z=.Z)[[1]]*100
 .out$ci_hi <- adjwald_ci(.x,.n,.Z=.Z)[[2]]*100
 .out$ci_method <- .ci_note
 .out$method_note <- c("small sample adjust")
  .out[,c(3,4,5,1,6,2,7:13)]
}

}

#'
success_vs_bench_smallsample<-function(.x,.n,.p){

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
            function(.xx,.p_=.p){
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
  .new$observed_success_pct <- (.x/.n)*100
  .new$success_exact_prob <- (100-(.new$pval_exact*100))
  .new$success_mid_prob <- (100-(.new$pval_mid*100))
  return(.new)
}


#' @examples
#' # Comparing 19 success/25 trials (users) to a 75% benchmark completion rate
#' success_vs_bench(19,25,0.75)
#'
#' .ux_data <-
#' data.frame(
#'  "id" = rep(seq(1,10,1),2),
#'  "task" = c(rep(1,10),rep(2,10)),
#'  "complete"  = sample(0:1,20,replace=TRUE,prob = c(.3,.65))
#' )
#'
#' success_vs_bench(.ux_data, complete, .p=0.7,task)
#' @rdname success_vs_bench
#' @param .var If \code{.x} is a long-format data frame, the (unquoted) name of a data frame column containing task success outcomes (as 1s and 0s, corresponding to successes and failures, respectively).
#' @param ... (Optional) If \code{.x} is a long-format data frame, you can pass the name of one or more grouping variables as unquoted, comma-separated column names (without naming the \code{...} argument) to compute stats by groups.

#' @export
success_vs_bench.data.frame<-function(.x,.var,.p, ...,.alt=c("greater","less","twotailed"),.alpha=0.05){

  .out <-
    dplyr::group_by(.x, ...)
  .out <-
    dplyr::summarise(
      .out,
      successes = sum({{ .var }}),
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

  return(.out[,c(1,4,5,6,2,7,3,8:14)])

}
