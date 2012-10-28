\name{bp2.find}
\alias{bp2.find}
\title{Find bp2 model objects by slot values}
\description{Find bp2 model objects that have particular value in particular slot. The slot must exists in the object.}
\usage{
bp2.find(objs, slotName, slotValue)
}

\author{Ossi Koivistoinen}
\note{}
\arguments{
  \item{objs}{List of objects or vector of object ids to search}
  \item{slotName}{The slot to search}
  \item{slotValue}{The value the slot must contain}	
}
\value{
Subset of objs that match the condition.
}

\examples{
controls <- bp2.get.instances.of(my.bp2@all.objects, "bp2.control")
activations <- bp2.find(controls, "controlType", "\"ACTIVATION^^<http://www.w3.org/2001/XMLSchema#string>\"")
}
