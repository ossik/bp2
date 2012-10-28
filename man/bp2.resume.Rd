\name{bp2.resume}
\alias{bp2.resume}
\title{Resume saved bp2 model}
\description{Populate internal data structures of bp2 package from existing \code{bp2.model} object. bp2 can only process one \code{bp2.model} object at a time and this function has to be calld to select the object. This function has to be used after loading \code{bp2.model} object from the disk.}
\usage{
bp2.resume(obj)
}

\author{Ossi Koivistoinen}
\note{}
\arguments{
  \item{obj}{bp2.model object to continue working on.}
}

\seealso{\code{\link{bp2.model}}, \code{\link{bp2.create.model}}}

\value{
}

\examples{
# Save bp.model for later processing
my.bp2 <- bp2.create.model(...)
save(my.bp2, file='my.bp2.model.RData')

# Resume working on old model
load('my.bp2.model.RData')
bp2.resume(my.bp2)
}
