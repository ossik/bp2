\name{bp2.get.instances.of}
\alias{bp2.get.instances.of}
\title{Filter particular class from list of objects}
\description{Returns instances of only one class from a list of objects of many different classes}
\usage{
bp2.get.instances.of(objs, desired.class.names)
}

\author{Ossi Koivistoinen}
\note{}
\arguments{
  \item{objs}{List of objects or object ids}
  \item{desired.class.names}{Vector of character strings containing the names of classes to select}	
}

\value{
Sublist of objs containg objects or object ids of particular class. The type of return value is the same than type of \code{objs} argument.
}

\seealso{\code{\link{bp2.model}}, \code{\link{bp2.create.model}}}

\examples{
my.bp2 <- bp2.create.model(...)
proteins <- bp2.get.instances.of(my.bp2@all.objects, "bp2.protein")
}
