\name{bp2.flatten}
\alias{bp2.flatten}
\title{Flatten lists of lists}
\description{Internal function. Get components of list of lists as numeric vector.}
\usage{
bp2.flatten(l)
}

\author{Ossi Koivistoinen}
\note{}
\arguments{
  \item{l}{List of numeric vectors}
}

\value{
Vector containing the unique components included in the components of \code{lis}. Names and other properties are dopped
}

\seealso{\code{\link{bp2.create.model}}}

\examples{
my.bp2 <- bp2.create.model(...)
pathways <- bp2.get.instances.of(my.bp2@all.objects, "bp2.pathway")
pwSteps <- bp2.find.followers(pathways, include.slots=''pathwayStep'') 
bp2.flatten(pwSteps)
}
