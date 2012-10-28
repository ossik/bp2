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



bp2.normalize.complexes <- function(complexes, mapping=NULL)
{
  sub.complexes <- vector("list",length=length(complexes))
  sub.proteins <- vector("list",length=length(complexes))
  names(sub.complexes) <- names(sub.proteins) <- names(complexes)
  
  for(i in 1:length(complexes))
    {
      if (i %% 100 == 0) { cat("Complex", i, "/", length(complexes),"\n") }
      physical.entity.participants <- bp2.flatten(bp2.find.followers(complexes[[i]], include.slots="components"))
      if (length(physical.entity.participants) > 0)
        {
          sub.phys.ents <- bp2.flatten(bp2.find.followers(physical.entity.participants,include.slots=c("physicalEntity")))
          sub.complexes[[i]] <- bp2.get.instances.of(sub.phys.ents, "bp2.complex")
          sub.proteins[[i]] <- bp2.get.instances.of(sub.phys.ents, "bp2.protein")
        }
      else
        {
          cat("No physical entity participants\n")
        }
    }
  
  sub.proteins <- bp2.find.proteins(sub.proteins)
  if(!is.null(mapping))
    {
      genes <- bp2.uniprot.to.entrez(sub.proteins, mapping)
      genes <- bp2.filter.NAs.from.list.of.lists(genes)
    }
  else
    {
      genes <- NULL
    }
  
  list(names=names(complexes),
       id=sapply(complexes, function(x) {slot(x, "rdf.id")}),
       sub.complexes=sub.complexes,
       proteins=sub.proteins,
       genes=genes)
}
