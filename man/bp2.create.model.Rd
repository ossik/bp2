\name{bp2.create.model}
\alias{bp2.create.model}
\title{Construct bp2.model based on RDF data}
\description{
This function creates BioPAX level 2 object model, \code{bp2.model}, based on biopax data given in RDF format. The RDF data is expected to be output from Rredland library. The model has been tested only with Reactome data.}
\usage{
my.bp2 <- bp2.create.model(rdf.data)
}

\author{Ossi Koivistoinen}
\note{}
\arguments{
  \item{rdf.data}{
\code{data.frame} containing the BioPAX level 2 data in rdf format. The data consists of three columns: subject, predicate and object. Subjects and objects are symbol names and predicates name the relations between subjects and objects. Object names can be identical to subjects in which case they are references to corresponding objects.}
}

\value{
Instance of "\link{bp2.model}" class.
}

\seealso{"\link{bp2.model}", \code{\link{bp2.resume}}, \code{\link{bp2.find.nodes}}}

\examples{
# 1. Read BioPAX data in RDF format using Rredland
require(Rredland)
biopax.file.path <- system.file('data/Reactome_69278.owl',package='bp2')
biopax.owl.file.uri <- paste('file://', biopax.file.path, sep='')	
biopax.rdf <- readRDF(biopax.owl.file.uri, storageType="bdb")
biopax.rdf.data.frame <- as(biopax.rdf, "data.frame")
freeRedl(biopax.rdf) 

# 2. Create bp2.model
my.bp2 <- bp2.create.model(biopax.rdf.data.frame)

# 3. Show some objects
my.bp2@all.objects[1:20]
}
