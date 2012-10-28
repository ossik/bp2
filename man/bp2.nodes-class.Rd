\name{bp2.nodes}
\docType{class}

\alias{bp2.nodes}

\alias{show,bp2.model-method}

\title{bp2.nodes class: Connections between pathways} 

\description{Description of connections between pathways and protein complexes. The objects of this class contain many lists whose members are either vectors of UniprotIDs or indexes of nodes within this object.

With the exception of \code{inverse.proteins} all list are of same length and position in the list is the index of the node. The bp2 model indexes and names of nodes are found in \code{ids} table.
}
\section{Objects from the class}{
  Returned by \code{\link{bp2.find.nodes}} function.
}
\section{Slots}{
  \describe{
    \item{ids}{List of numeric numeric bp2 model id of objects. The numbers in all other lis are indexes of this list. Thus, this list can be used to find corresponding objects in bp2.all.objects table}

\item{proteins}{List of character vectors of Uniprot IDs in this particular node}

\item{inverse.proteins}{Special list whose indexes are all uniprot IDs that appear in the model and values are list of node indexes that contain the protein}

\item{sub.nodes}{List of vectors of other node indexes that are included in the nodes.} 

\item{nodes.with.common.proteins}{List of vectors of other node indexes that share proteins  with the nodes.}

\item{nodes.with.activated.proteins}{List of vectors of other node indexes that are share protein through activation relation with the nodes.}

\item{nodes.with.inhibited.proteins}{List of vectors of other node indexes that are share protein through inhibition relation with the nodes.}

\item{activated.nodes}{List of vectors of other node indexes that are activated by the nodes.}

\item{activated.proteins}{List of character vectors Uniprot IDs that are activated by the nodes.}

\item{inhibited.nodes}{List of of other node indexes that are inhibited by the nodes.}

\item{inhibited.proteins}{List of character vectors Uniprot IDs that are inhibited by the nodes.}

\item{all.followers}{Union of sub.nodes, activated.nodes, inhibited.nodes and common.proteins }

\item{all.inhibited}{Union of inhibited.nodes and nodes.with.inhibited.protein}

}
}

\author{Ossi Koivistoinen}
\seealso{\code{\link{bp2.find.nodes}}}


