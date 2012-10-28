\name{bp2.verbose}
\alias{bp2.verbose}
\title{Set verbosity of bp2 pacakge}
\description{Affects level of screen output of many functions in bp2.package.}
\usage{
bp2.verbose(new.value=NULL)
}

\author{Ossi Koivistoinen}
\note{}
\arguments{
  \item{new.value}{New boolean value for verbosity. If null only return current value.}

}

\value{
Previous value of verbosity setting
}

\seealso{\code{\link{bp2.create.model}}}

\examples{
bp2.verbose(TRUE)
}
