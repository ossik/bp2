## Copyright (c) 2010 Ossi Koivistoinen

## Permission is hereby granted, free of charge, to any person obtaining a copy
## of this software and associated documentation files (the "Software"), to deal
## in the Software without restriction, including without limitation the rights
## to use, copy, modify, merge, publish, distribute, sublicense, and/or sell
## copies of the Software, and to permit persons to whom the Software is
## furnished to do so, subject to the following conditions:

## The above copyright notice and this permission notice shall be included in
## all copies or substantial portions of the Software.

## THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS OR
## IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF MERCHANTABILITY,
## FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT. IN NO EVENT SHALL THE
## AUTHORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER
## LIABILITY, WHETHER IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING FROM,
## OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER DEALINGS IN
## THE SOFTWARE.



bp2.countClassInstances <- function(objects)
{
  table(sapply(objects,class))
}

# e.g. "Q13255^^<http://www.w3.org/2001/XMLSchema#string>" to "Q13255"
bp2.simplifyString <- function(longValue)
  {
    return(sub("\"", "", sub("\\^\\^.*", "", longValue)))
  }



bp2.getUniprotClusters <- function(pathways)
  {
    pathwayIds <- sapply(pathways, function(x){slot(x, 'rdf.id')})
    physicalEntityClusters <- bp2.findPathwayPhysicalEntities(pathways)
    proteinClusters <- sapply(physicalEntityClusters, function(x){x[(which(sapply(bp2.all.objects[x],class) == "bp2.protein"))]})
    uniprotIds <- lapply(proteinClusters, function(cluster)
                          {
                            if(length(cluster) > 0)
                              {
                                uniprotIds <- bp2.getUniprotId(bp2.all.objects[cluster])
                                return(levels(uniprotIds[,2]))
                              }
                          }
                          )
    names(uniprotIds) <- pathwayIds
    uniprotIds
  }









