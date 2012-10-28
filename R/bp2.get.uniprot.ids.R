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



# Input: List of objects of class bp2.protein
# Output: data.frame with two columns.
#   Column 1: numeric rdf.id of protein
#   Column 2: Uniprot id string
#   rownames: symbolic rdf.id
bp2.get.uniprot.ids <- function(proteins)
{
  uniprot.key = match("\"UniProt^^<http://www.w3.org/2001/XMLSchema#string>\"", .bp2[["symbol.names"]])
  uniprot.ids <- c()
  index <- 0
  for(p in proteins)
    {
      index <- index + 1
      xrefs = slot(p, 'xref')
      for(j in .bp2[["all.objects"]][xrefs])
        {
          if (is(j, 'bp2.unificationXref'))
            {
              if(slot(j,'db') == uniprot.key)
                {
                  uniprot.value.key <- slot(j,'id')
                  uniprot.value <- .bp2[["symbol.names"]][uniprot.value.key]
                  uniprot.ids[index] <- bp2.simplifyString(uniprot.value)
                }
            }
          else
            {
               uniprot.ids[index] <- NA
            }
        }
    }
  mapping <- data.frame(protein=bp2.objects2ids(proteins), uniprot.id=uniprot.ids)
  mapping
}
