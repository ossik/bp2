\name{bp2.minimum.spanning.tree}
\alias{bp2.minimum.spanning.tree}
\alias{bp2.print.tree}
\title{Solve minimum spanning trees for follower lists}

\usage{
tree <- bp2.minimum.spanning.tree(follower.list, root, max.depth=length(follower.list))
bp2.print.tree(tree)
}

\description{Solves minimum spanning tree for a graph of BioPAX classes based on follower list returned by \code{bp2.find.followers}. This is useful for solving minimum distances between model objects. The types of relations that are considered are selected }

\author{Ossi Koivistoinen}
\note{}

\arguments{
  \item{follower.list}{Follower list representation of bp2 model object ids as returned by \code{bp2.follower.list objects.}}
  \item{root}{Index of root vertex in follower.list}
  \item{max.depth}{Maximum distance from root, after which the algorithm is terminated.}
\item{tree}{Spanning tree \code{data.frame} returned by \code{bp2.minimum.spanning.tree}}
}

\value{
Returns data.frame that contains columns of same length than \code{follower.list}: \code{id}, \code{parent} and \code{distance}. \code{id} is the index in the \code{follower.list}, \code{parent} is the id of parent vertex for the given id and \code{dist} is the distance from root.
}

\note{
This function is not very useful for general use until \code{bp2.find.followers} supports configuration of \code{include.slots} argument separately for different class types. Use \code{\link{bp2.find.nodes}} for constructing interesting graphs based on BioPAX data.
}

\examples{
my.bp2 <- bp2.create.model(...)
complexes <- bp2.get.instances.of(my.bp2@all.objects, "bp2.complex")
followers <- bp2.find.followers(pathways, include.slots=''components,physicalEntity'') 
tree <- bp2.minimum.spanning.tree(followers, 1)
bp2.print.tree(tree)
}
