\name{bp2.find.nodes}
\alias{bp2.find.nodes}
\title{Solve dependencies between pathways and complexes}
\description{Creates a list of nodes and lists of dependencies between the nodes by recursiely traversing given pathways and complexes within the active bp2.model. The collected data includes list of proteins that are directly linked to each pathway and protein and lists of different types of connections between the nodes. These lists can be used ot construct graphs of nodes.}
\usage{
nodes <- bp2.find.nodes(pathways, complexes)
}

\author{Ossi Koivistoinen}
\note{}
\arguments{
  \item{pathways}{ List of "bp2.pathway" objects used as nodes. Can be empty.}
  \item{complexes}{ List of "bp2.complex" objects used as nodes. Can be empty.}	
}

\value{
Instance of "bp2.nodes" class.
}

\seealso{"\link{bp2.nodes}", \code{\link{bp2.create.model}}}

\examples{
my.bp2 <- bp2.create.model(...)
pathways <- bp2.get.instances.of(my.bp2@all.objects, "bp2.pathway")
complexes <- bp2.get.instances.of(my.bp2@all.objects, "bp2.complex")
nodes <- bp2.find.nodes(pathways, complexes)
}
