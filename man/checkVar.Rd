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


\name{checkVar}
\alias{checkVar}
\title{
Summary statistics from named variable of a data.frame
}
\description{
Summary statistics calculated from named variable of a data.frame.
The named variable must be in numeric form with no missing values.

Summary statistics include: mean, standard deviation (sd), interquartile range (IQR), percentile value (0, 25, 50, 75 and 100 percentiles) and count (length)
}
\usage{
checkVar(varname, data)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{varname}{Variable name of data.set. The named variable must be in numeric form with no missing values.
  \item{data}{data.frame object}
}

\examples{
checkVar("Sepal.Length", iris)

##                 mean        sd IQR `0%` `25%` `50%` `75%` `100%`   n
## Sepal.Length 5.843333 0.8280661 1.3  4.3   5.1   5.8   6.4    7.9 150

#to loop through all numerical variables in iris:

num.vars <- names(iris)[-ncol(iris)]
chvar.list <- lapply(num.vars, checkVar, data=iris)
do.call(rbind, chvar.list)

##                  mean        sd IQR `0%` `25%` `50%` `75%` `100%`   n
## Sepal.Length 5.843333 0.8280661 1.3  4.3   5.1  5.80   6.4    7.9 150
## Sepal.Width  3.057333 0.4358663 0.5  2.0   2.8  3.00   3.3    4.4 150
## Petal.Length 3.758000 1.7652982 3.5  1.0   1.6  4.35   5.1    6.9 150
## Petal.Width  1.199333 0.7622377 1.5  0.1   0.3  1.30   1.8    2.5 150

}

\author{
Adam Quek
}
