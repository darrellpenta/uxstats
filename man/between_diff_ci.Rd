% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/between_diff_ci-function.R
\name{between_diff_ci}
\alias{between_diff_ci}
\title{Computes confidence intervals on the difference of means for between-subjects tests}
\usage{
between_diff_ci(
  .x,
  .sd,
  .n,
  .x2,
  .sd2,
  .n2,
  .t,
  .return = c("both", "low", "hi")
)
}
\arguments{
\item{.x}{the mean from sample 1.}

\item{.sd}{the standard deviation from sample 1.}

\item{.n}{the size of sample 1.}

\item{.x2}{the mean from sample 2.}

\item{.sd2}{the standard deviation from sample 2.}

\item{.n2}{the size of sample 2.}

\item{.t}{the critical T value}

\item{.return}{One of \code{c("both","low","hi")}. Defaults to \code{both}, returning a list with the lower and upper limits}
}
\value{
a list with the lower and upper confidence limits, or either limit if \code{.return} is specified.
}
\description{
Computes confidence intervals on the difference of means for between-subjects tests
}
\seealso{
Other point estimate helpers: 
\code{\link{adjwald_ci}()},
\code{\link{tdist_ci}()},
\code{\link{within_diff_ci}()}
}
\concept{point estimate helpers}
