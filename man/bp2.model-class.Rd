\name{bp2.model}
%\Rdversion{1.1}
\docType{class}

\alias{bp2.model}

\alias{show,bp2.model-method}


\title{bp2.model class: S4 object model of BioPAX level 2} 

\description{"bp2.model" object contains the representaition of BioPAX
level 2 databases as S4 objects. There is one S4 object for each
BioPAX object. BioPAX objects are stored in
code{bp2.model@all.objects} list.

The classes in the model are: bp2.biochemicalReaction
\code{bp2.biochemicalReaction}, 
\code{bp2.bioSource}, 
\code{bp2.catalysis}, 
\code{bp2.chemicalStructure},
\code{bp2.complex},
\code{bp2.complexAssembly},
\code{bp2.confidence},
\code{bp2.control},
\code{bp2.conversion},
\code{bp2.dataSource},
\code{bp2.dna},
\code{bp2.entity},
\code{bp2.evidence},
\code{bp2.experimentalForm},
\code{bp2.externalReferenceUtilityClass},
\code{bp2.interaction},
\code{bp2.modulation},
\code{bp2.openControlledVocabulary},
\code{bp2.pathway},
\code{bp2.pathwayStep},
\code{bp2.physicalEntity},
\code{bp2.physicalEntityParticipant},
\code{bp2.physicalInteraction},
\code{bp2.protein},
\code{bp2.publicationXref},
\code{bp2.relationshipXref},
\code{bp2.rna},
\code{bp2.sequenceFeature},
\code{bp2.sequenceInterval},
\code{bp2.sequenceLocation},
\code{bp2.sequenceParticipant},
\code{bp2.sequenceSite},
\code{bp2.smallMolecule},
\code{bp2.transport},
\code{bp2.transportWithBiochemicalReaction},
\code{bp2.unificationXref},
\code{bp2.utilityClass},
and \code{bp2.xref}.

Each class contains all slots described in BioPAX level 2 specification in
Java-style capiatalization, for example class "bp2.pathway" has slot
"pathwayComponents". All slots that contains references to other
objects also have corresponding slots for inverse relations on the
receiving objects, for example "bp2.pathwayStep" objects contain slot
"pathwayComponents.of" that contains references to all "bp2.pathways"
that refer to this "bp2.pathwayStep". 

The \code{bp2} package uses its own reference system implemented
similar to R factors: All fields of all objects in \code{bp2.model} are
vectors of integers. These integers are indexes of
\code{@symbol.names} list within the \code{bp2.model} object. The names of
\code{@all.objects} list are located in the beginning of \code{@symbol.names}
list. Thus, if there is integer value in any object that is less than
\code{length(my.bp2@all.objects)} it is a reference to the object
with that location in \code{@all.objects} list and the name of that object is
found with the same index in \code{@symbol.names} list. If the integer is
larger, then it is just a symbol name, not a reference to another
object and the string value of the symbol is found in symbol.names
list.

The show method for the objects automatically retrieves the names from
the symbol name list so printing a objects gives human readable
output.
}
\section{Objects from the class}{
  Returned by \code{\link{bp2.create.model}} function.
}
\section{Slots}{
  \describe{
    \item{all.objects}{List of all BioPAX objects contained in the model. The root class of all model classes is virtual \code{bp2.rdf.entity}. There is concrete class for each class in BioPAX level 2 specification. }

\item{symbol.names}{List of all symbol names that appear either as subjects or objects in RDF data. See Description}

\item{predicate.names}{All predicate names that appreared in RDF data. These are converted to object slots.} 

\item{all.slot.names}{Character vector of all slot names in objects known to bp2.model..} } }

\section{Methods}{
  \describe{
    \item{show}{\code{signature(x = "bp2.model")}: Prints the slot names of the object}
    \item{show}{\code{signature(x = "bp2.rdf.entity")}: Prints human readable presentation of model objects}
  }
}

\author{Ossi Koivistoinen}
\seealso{\code{\link{bp2.create.model}}}


