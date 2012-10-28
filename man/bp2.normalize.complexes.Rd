\name{bp2.normalize.complexes}
\alias{bp2.normalize.complexes}
\title{Solve proteins that form protein complexes}
\description{Expands list of "bp2.complex" objects to list of Uniprot ID character vectors.}
\usage{
bp2.normalize.complexes(complexes, mapping=NULL)
}

\author{Ossi Koivistoinen}
\note{}
\arguments{
  \item{complexes}{List of bp2.complex objects}

}

\value{
List of character verctors same length with \code{complexes}. Each vector contains the UniprotIDs included in the corresponding comples
}

\seealso{\code{\link{bp2.find.nodes}}}

\examples{
my.bp2 <- bp2.create.model(...)
complexes <- bp2.get.instances.of(my.bp2@all.objects, "bp2.complex")
uniprot.complexes <- bp2.normalize.complexes(complexes)
}
