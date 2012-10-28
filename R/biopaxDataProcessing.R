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


# Return: List of ID vectors
bp2.get.slot <- function(objs, slot.name)
{
  sapply(objs, function(obj)
    {
      if (match(slot.name, slotNames(obj), nomatch = 0) > 0)
        {
          return(slot(obj, slot.name))
        }
      else
        {
          return(NA)
        }
    })
}

bp2.get.instances.of <- function(objs, desired.class.names)
  {
    if(is.numeric(objs))
      {
        class.names <- sapply(.bp2[["all.objects"]][objs], class)
        objs[(class.names %in% desired.class.names)]
      }
    else
      {
        class.names <- sapply(objs, class)
        objs[(class.names %in% desired.class.names)]
      }
  }

# Return: Filtered vector of data objects, in which slotName has slotValue
bp2.find <- function(objs, slotName, slotValue)
{
  if (is.numeric(slotValue))
    {
      slotValueId = slotValue
    }
  else
    {
      slotValueId = match(slotValue, .bp2[["symbol.names"]], nomatch = 0)
    }
  slotValues <- bp2.get.slot(objs, slotName)
  correctValues <- sapply(slotValues, function(e){ slotValueId %in% e})
  objs[correctValues]
}

bp2.flatten <- function(lis)
  {
    unique(as.vector(c(lis,recursive=T)))
  }

bp2.find.followers <- function(objs, include.slots = NA, exclude.slots = NA)
  {
    if(is.numeric(objs))
      {
        obj.ids <- objs
      }
    else
      {
        obj.ids <- bp2.objects2ids(objs)
      }
    follower.list <- vector(mode="list")
                                        # Choose slots to use to build the table
    if (!is.na(include.slots[1]))
      {
        slots = include.slots
      }
    else
      {
        slots <- bp2.get.all.slot.names()
        if (!is.na(exclude.slots[1]))
          {
            slots <- slots[!(slots %in% exclude.slots)]
          }
      }
    pos <- 0
    for(id in obj.ids)
      {
        pos <- pos + 1
        if (pos%%1000 == 0)
          {
            cat(paste(date(), ":", pos, "/", length(obj.ids),"\n"))
          }
        followers <- vector(mode="integer",0)
        obj <- .bp2[["all.objects"]][[id]]
        if(.bp2[["verbose"]])
          {
            cat(.bp2[["symbol.names"]][id],"\n")
          }
        obj.slots <- slots[slots %in% slotNames(obj)]
        for(s in obj.slots)
          {
            values <- slot(obj, s)
            references <- values[bp2.exists(values)]
            if (length(references))
              {
                if (.bp2[["verbose"]])
                  {
                    cat(paste("    ",s, "=>", paste(references, collapse=" "), "\n"))
                  }
                followers <- c(followers,references)
              }
          }
        follower.list[[pos]] <- unique(followers)
      }
    names(follower.list) <- obj.ids
    follower.list
  }


