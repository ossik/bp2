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



bp2.minimum.spanning.tree <- function(follower.list, root, max.depth=length(follower.list))
   # Mark Allen Weiss, Data Structures & Algorithm Analysis in Java figure 9.16
  {
    size <- length(follower.list)
    dist <- rep.int(Inf, size)
    known <- rep(FALSE, size)
    path <- vector("integer", size)
    dist[root] <- 0
    for (currDist in bp2.sequence(0, max.depth))
      {
        found_any <- FALSE
        for(i in 1:size)
          {
            if (known[i] == FALSE && dist[i] == currDist)
              {
                found_any <- TRUE
                known[i] <- TRUE
                for (k in bp2.sequence(1, length(follower.list[[i]])))
                  {
                    if (dist[ follower.list[[i]] [k] ] == Inf)
                      {
                        dist[ follower.list[[i]] [k] ] <- currDist + 1
                        path[ follower.list[[i]] [k] ] <- i
                      }
                  }
              }
          }
        if (!found_any)
          {
            break
          }
      }
    spanning.tree=data.frame(id=1:size, parent=path, distance=dist)
    if(.bp2[["verbose"]])
      {
        bp2.print.tree(spanning.tree)
      }
    return(spanning.tree)
  }


bp2.print.tree <- function(tree)
  {
    otree <- tree[ order(tree$dist, tree$parent, tree$id),]
    cat("Distances from    ", otree$id[1], ",   ", class(.bp2[["all.objects"]][[ otree$id[1] ]]), ",    ", .bp2[["symbol.names"]][ otree$id[1] ], "  (root)\n", sep="")
    for (i in bp2.sequence(2,length(otree$id)))
      {
        if(otree$distance[i] < Inf)
          {
            cat(otree$id[i],
                "  parent id  ",
                otree$parent[i] ,
                "  distance  ",
                otree$distance[i],
                "  name  ",
                .bp2[["symbol.names"]][ otree$id[i] ],
                "  class  ",
                class(.bp2[["all.objects"]][[ otree$id[i] ]]),
                
                "\n", sep="")
          }
      }
  }
