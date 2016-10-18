\name{findTerm}
\alias{findTerm}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{Find User-specified Term in Files}
\description{findTerm function look for user-specified term across files within named files}
\usage{
findTerm(x, file.list, full = "n")
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{x}{A single user specified term (string). Specified term need not be case-specific}
  \item{file.list}{List of files generated with base list.files function}
  \item{full}{Option for returning full search string ("y") or return only lines matching term x ("n")}
}

\value{
Return list of lines which specified term x occur
}

\details{
%%  ~~ If necessary, more details than the description above ~~
}

\examples{
cat("Lorem ipsum dolor sit amet,\n consectetur adipiscing elit.\n Sed tempor sem id enim molestie viverra.",
    file="lorem.R")
cat("Maecenas convallis sagittis aliquet.\n Cras porttitor commodo risus a laoreet.\n Maecenas urna sapien, pellentesque et blandit sed, molestie quis ipsum.",
    file="ipsum.R")
file.list <- list.files(pattern="\\\.r$", ignore.case=T)
findTerm("ipsum", file.list)
unlink(c("lorem.R", "ipsum.R"))
}
