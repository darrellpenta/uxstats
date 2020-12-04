#' Calculate the degrees of freedom for a between-subjects design.
#'
#' @description
#' \href{https://g.co/kgs/a7Zyyn}{Sauro and Lewis (2012)} recommend using the Welch-Satterthwaite procedure for computing the degrees of freedom in a between-subjects design.
#'
#' @param .sd  the standard deviation from sample 1.
#' @param .n  the size of sample 1.
#' @param .sd2  the standard deviation from sample 2.
#' @param .n2  the size of sample 2.
#' @return Returns the between-subjects degrees of freedom
#' @family t-distribution functions (~ Excel)
#' @rdname between_df
#' @export
#'
between_df <-
  function(.sd,.n,.sd2,.n2){
    if(!is.numeric(.sd)){
      stop(".sd must be a numeric")
    }
    if(!is.numeric(.sd2)){
      stop(".sd2 must be a numeric")
    }
    if(!is.numeric(.n)){
      stop(".n must be a numeric")
    }
    if(!is.numeric(.n2)){
      stop(".n2 must be a numeric")
    }
    if(.n<=0){
      stop(".n must be greater than 0")
    }
    if(.n2<=0){
      stop(".n2 must be  greater than 0")
    }
   .num<- ((.sd^2/.n) + (.sd2^2/.n2))^2
   .denom1 <- (.sd^2/.n)^2/(.n-1)
   .denom2 <- (.sd2^2/.n2)^2/(.n2-1)

    .df<-.num/(.denom1+.denom2)
    return(floor(.df))

  }
