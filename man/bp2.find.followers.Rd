\name{bp2.find.followers}
\alias{bp2.find.followers}
\title{Construct follower lists for bp2.model objects}
\usage{
bp2.find.followers(objs, include.slots=NA, exclude.slots=NA)
}
\description{List slot values of bp2 model objects. Returns value is a follower list for	objs that can be processed further with different graph packages}

\author{Ossi Koivistoinen}
\note{}
\arguments{
  \item{objs}{
List of bp2 model objects or vector of model object ids in the active model.
  }
  \item{include.slots}{
If value other than NA is specified get only valued of names slots. Otherwise use all slots.
  }
  \item{exclude.slots}{
	Don't read the values of these slots.
  }
}

\value{ Returns list that contains one entry for each object id
provided as an argument. Each list entry is a vector of integers that
are IDs of other objects that are connected to the object.  }

\seealso{\code{\link{bp2.minimum.spanning.tree}}, \code{\link{bp2.create.model}}}

\examples{
# Create follower list for all objects using all possible connections
my.bp2 <- bp2.create.model(...)
follower.list <- bp2.find.followers(my.bp2@all.objects) 

# See how many different control types there are
controls <- bp2.get.instances.of(my.bp2@all.objects, "bp2.control")
control.types <- bp2.find.followers(controls, include.slots=''controlType'')
table(control.types)
}
