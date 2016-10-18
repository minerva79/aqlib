\name{lib}
\alias{lib}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{Load Multiple-Defined Packages}
\description{Loading multiple packages onto parent environment. Packages are pre-defined for data wrangling, data visualisation and non-linear regression purposes. }
\usage{
lib(type = c("dat", "ggp2", "mgcv"))
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{type}{data wrangling packages 'dat' load 'plyr', 'dplyr' and 'tidyr';
  data visualisation packages 'ggp2' load 'ggplot2', 'grid' and 'gridExtra';
  non-linear regression packages 'mgcv' load 'mgcv' and 'visreg' packages}
}

\examples{
# to load data wrangling packages
lib("dat")

# to load all 8 pre-defined packages (plyr, dplyr, tidyr, ggplot2, grid, gridExtra, mgcv, visreg) sequentially
lib()
