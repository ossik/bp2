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



# filter NAs and clear duplicates
bp2.filter.NAs.from.list.of.lists <- function(l)
  {
    listWithoutNAs <- list()
    notNAs <- lapply(lapply(lapply(l, is.na),function(x){x==F}), which)
    for (i in 1:length(l))
    {
      listWithoutNAs[[i]] <- unique(l[[i]][notNAs[[i]]])      
    }
    names(listWithoutNAs) <- names(l)
    listWithoutNAs
  }

# filter NAs and clear duplicates
bp2.filter.NAs.from.list <- function(list)
  {
    notNAs <- sapply(sapply(list, is.na), function(x){x==F})
    list <- unique(list[notNAs])
    list
  }
