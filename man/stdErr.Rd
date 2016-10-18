\name{stdErr}
\alias{stdErr}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{Standard Error
}
\description{
Calculate standard error
}
\usage{
stdErr(x)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{x}{a vector}
}

\details{sqrt(var(x,na.rm=TRUE)/length(na.omit(x)))
Standard error returned as the square root of division between variance of x and length of x. Missing values (NA) within the vector omitted prior to calculation}

\examples{
x <- rnorm(50, mean=5, sd=1)
stdErr(x)
