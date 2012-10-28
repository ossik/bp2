\name{bp2.get.slot}
\alias{bp2.get.slot}
\title{Read slot values}
\description{Internal function. }
\usage{
bp2.get.slot(objs, slot.name)
}

\author{Ossi Koivistoinen}
\note{}
\arguments{
  \item{objs}{
	List of S4 objects, whose named slot is read
  }
  \item{slot.name}{
        Name of the slot to read 
  }
}

\value{ List of same length with objs that contains the values for the
	slot for objs. Return NA instead of list if no objects
	contained the named slot.  }

\examples{
my.bp2 <- bp2.create.model(...)
bp2.get.slot(bp2@all.objects, "rdf.id")
}
