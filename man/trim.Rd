\name{trim}
\alias{trim}
\alias{trim.trailing}
\alias{trim.leading}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{Trim trailing or leading white space}
\description{Remove white spaces in before (leading) or after (trailing) string character}
\usage{
trim(x)
trim.trailing(x)
trim.leading(x)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{x}{string character}
}

\examples{
v1 <- c("michael moore ", " michael moore")
trim(v1)
trim.leading(v1)
trim.trailing(v1)
}

\author{
Adam Quek
}
