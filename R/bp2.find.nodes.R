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


# How to get from pathway to physical entity. Legend: (class), property, [class, not used]
#
# (pathway) -> pathwayComponents -> (pathwayStep)     -> nextStep          -> (pathwayStep)
#                                                     -> stepInteractions  -> (pathway)
#                                                                          -> (interaction)
# (interaction)                  -> participants      -> (physicalEntityParticipant)
# (interaction::conversion)      -> left              -> (physicalEntityParticipant)
#                                -> right             -> (physicalEntityParticipant)
# (interaction::control)         -> controller        -> (physicalEntityParticipant)
#                                -> controlled        -> (pathway)
#                                                     -> (interaction)        
# (physicalEntityParticipant)    -> physicalEntity    -> (physicalEntity)


bp2.find.nodes <- function(pathways, complexes)
  {
    # Global variables for nested functions
    # These lists are populated by nested functions during recursion
    pw.physical.entities <- vector("list",length=length(pathways))
    pw.connections.neutral <- vector("list",length=length(pathways))
    pw.connections.activation <- vector("list",length=length(pathways))
    pw.connections.inhibition <- vector("list",length=length(pathways))
    elem.connections.activation <- vector("list",length=length(pathways))
    elem.connections.inhibition <- vector("list",length=length(pathways))
    names(pw.physical.entities) <- names(pw.connections.neutral) <- names(pw.connections.activation) <- names(pw.connections.inhibition) <- names(elem.connections.activation) <- names(elem.connections.inhibition) <- names(pathways)
    bp2.pathway.steps.seen <- c()
    this.pw.index <- 0
    
    bp2.find.pathway.list.physical.entities <- function(pathways, max.level=Inf, level=0)
      {
        if (level > max.level) { return(c()) }
        for(index in 1:length(pathways))
          {
            if (index %% 100 == 0) { cat(format(" ", width=level), "Pathway", index, "/", length(pathways),"\n")}
            bp2.pathway.steps.seen <<- c()
            this.pw.index <<- index
            phys.ents <- bp2.flatten(bp2.find.pathway.physical.entities(pathways[[index]], max.level, level+1))
            if (!is.null(phys.ents))
              {
                pw.physical.entities[[index]] <<- phys.ents
              }
          }
      }
    
    bp2.find.pathway.physical.entities <- function(pathway, max.level=Inf, level=0)
      {
        if (level > max.level) { return(c()) }
        if (length(pathway) < 1) { return(c()) }
                                        #cat(format(" ", width=level), "+Pathway","\n")
        
        phys.ents <- c()
        pathway.steps <- bp2.flatten(bp2.find.followers(pathway,include.slots=c("pathwayComponents")))
                                        #cat(format("", width=level), "-Pathway steps: ", length(pathway.steps), "\n")
        phys.ents <-  bp2.find.pathway.step.physical.entities(bp2.ids2objects(pathway.steps), max.level, level+1)
        phys.ents
      }
    
    bp2.find.pathway.step.physical.entities <- function(pathway.steps, max.level=Inf, level=0)
      {
        if (level > max.level) { return(c()) }
        if (length(pathway.steps) < 1) { return(c()) }
                                        #cat(format(" ", width=level), "+Pathway Step","\n")
        
        pathway.steps <- unname(as.vector(c(pathway.steps, recursive=T)))
        bp2.pathway.steps.seen <<- c(bp2.pathway.steps.seen, bp2.objects2ids(pathway.steps))
        
        next.step.phys.ents <- interaction.phys.ents <- c()
        
        step.interactions <- bp2.flatten(bp2.find.followers(pathway.steps,include.slots=c("stepInteractions")))
                                        #cat(format("", width=level), "-Step Interactions: ", length(step.interactions), "\n")
        interaction.phys.ents <- bp2.find.step.interaction.physical.entities(bp2.ids2objects(step.interactions), max.level, level+1)
        
        next.steps <- bp2.flatten(bp2.find.followers(pathway.steps,include.slots=c("nextStep")))
                                        #cat(format("", width=level), "-Next steps: ", length(next.steps), "\n")
        next.steps <- next.steps[!(next.steps %in% bp2.pathway.steps.seen)]
        next.step.phys.ents <- bp2.find.pathway.step.physical.entities(bp2.ids2objects(next.steps), max.level, level+1)
        
        c(next.step.phys.ents, interaction.phys.ents)
      }
    
    bp2.find.step.interaction.physical.entities <- function(step.interactions, max.level=Inf, level=0)
      {
        if (level > max.level) { return(c()) }
        if (length(step.interactions) < 1) { return(c()) }
                                        #cat(format(" ", width=level), "+Step Interaction","\n")
        
        step.interactions <-  unname(as.vector(c(step.interactions, recursive=T)))
        pw.phys.ents <- ia.phys.ents <- c()
        
        step.interaction.pathways <- bp2.flatten(step.interactions[(sapply(step.interactions,class) == "bp2.pathway")])
                                        #cat(format("", width=level), "-Pathways: ", length(step.interaction.pathways), "\n")

        if(length(step.interaction.pathways) > 0) 
          {
            pw.connections.neutral[[this.pw.index]] <<- c(pw.connections.neutral[[this.pw.index]], bp2.objects2ids(step.interaction.pathways))
           #pw.phys.ents <- bp2.flatten(bp2.find.pathway.physical.entities(step.interaction.pathways, max.level, level+1))
          }

        
        step.interaction.interactions <- bp2.flatten(step.interactions[(sapply(step.interactions, function(c){inherits(c,"bp2.interaction")}))])
#cat(format("", width=level), "-Interactions: ", length(step.interaction.interactions), "\n")
        ia.phys.ents <- bp2.flatten(bp2.find.interaction.physical.entities(step.interaction.interactions, max.level, level+1))
        c(pw.phys.ents, ia.phys.ents)
      }
    
    activation.controls <- match(c("\"ACTIVATION^^<http://www.w3.org/2001/XMLSchema#string>\"",
                                   "\"ACTIVATION-ALLOSTERIC^^<http://www.w3.org/2001/XMLSchema#string>\""),.bp2[["symbol.names"]])
    inhibition.controls <- match(c("\"INHIBITION^^<http://www.w3.org/2001/XMLSchema#string>\"",
                                   "\"INHIBITION-ALLOSTERIC^^<http://www.w3.org/2001/XMLSchema#string>\"",
                                   "\"INHIBITION-COMPETITIVE^^<http://www.w3.org/2001/XMLSchema#string>\"",
                                   "\"INHIBITION-NONCOMPETITIVE^^<http://www.w3.org/2001/XMLSchema#string>\""),.bp2[["symbol.names"]])
    
    bp2.find.interaction.physical.entities <- function(interactions, max.level=Inf, level=0)
      {
        if (level > max.level) { return(c()) }
        if (length(interactions) < 1) { return(c()) }
#cat(format(" ", width=level), "+Interaction","\n")
        
        interactions <- unname(as.vector(c(interactions, recursive=T)))
        phys.ents <- c.phys.ents <- c()
        interaction.controls <- bp2.flatten(interactions[(sapply(interactions, function(c){inherits(c,"bp2.control")}))])
#cat(format("", width=level), "-Controls: ", length(interaction.controls), "\n")
        for (control in interaction.controls)
          {
            controlled <- bp2.ids2objects(bp2.get.slot(list(control), "controlled"))
            type <- bp2.get.slot(list(control), "controlType")[1]
            if (length(type) > 1)
              {
                print(control)
              }
            if(is(controlled[[1]], "bp2.pathway"))
              {
                if (type %in% activation.controls)
                  {
                    pw.connections.activation[[this.pw.index]] <<- c( pw.connections.activation[[this.pw.index]], bp2.objects2ids(controlled))
                  }
                else if (type %in% inhibition.controls)
                  {
#cat("control ", class(control), " INHIBITION: ",  bp2.objects2ids(controlled), "\n");
                    pw.connections.inhibition[[this.pw.index]] <<- c(pw.connections.inhibition[[this.pw.index]], bp2.objects2ids(controlled))
                  }
                else
                  {
                    stop(paste("Unexpected control type", type))
                  }
              }
            else
              {
                controlled.phys.ents <- bp2.find.step.interaction.physical.entities(controlled, max.level, level+1)
                if (length(controlled.phys.ents) > 0)
                  {
                    if (type %in% activation.controls)
                      {
#cat("control ", class(control), " ACTIVATION: ", bp2.objects2ids(controlled), "\n");
                        elem.connections.activation[[this.pw.index]] <<- c( elem.connections.activation[[this.pw.index]], controlled.phys.ents)
                      }
                    else if (type %in% inhibition.controls)
                      {
                                        #cat("control ", class(control), " INHIBITION: ",  bp2.objects2ids(controlled), "\n");
                        elem.connections.inhibition[[this.pw.index]] <<- c(elem.connections.inhibition[[this.pw.index]], controlled.phys.ents)
                      }
                    else
                      {
                        stop(paste("Unexpected control type", type))
                      }
                  }
              }
          }
        # Find physical entities with no known activation/inhibition relelation to the current pathway 
        interaction.participants <- bp2.flatten(bp2.find.followers(interactions,include.slots=c("participants","left","right","controller")))
        #cat(format("", width=level), "-Participants,Lefts,Rights,Controllers: ", length(interaction.participants), "\n")
        phys.ents <- bp2.flatten(bp2.find.followers(interaction.participants,include.slots=c("physicalEntity")))
        c(phys.ents, c.phys.ents)
      }
  
    
    # Main

    # Find connection lists
    bp2.find.pathway.list.physical.entities(pathways)

    # Process pw processing results

    # These list don't need pre processing. They contain rdf.ids of other pathways
    # pw.connections.neutral
    # pw.connections.activation
    # pw.connections.inhibition

    pw.sub.complexes <- lapply(pw.physical.entities, function(x) {bp2.get.instances.of(x, "bp2.complex")})
    pw.act.complexes <- lapply(elem.connections.activation, function(x) {bp2.get.instances.of(x, "bp2.complex")})
    pw.inh.complexes <- lapply(elem.connections.inhibition, function(x) {bp2.get.instances.of(x, "bp2.complex")})

    pw.proteins <- bp2.find.proteins(pw.physical.entities)
    act.proteins<- bp2.find.proteins(elem.connections.activation)
    inh.proteins <-  bp2.find.proteins(elem.connections.inhibition)

    # Process complexes
    ncomplexes <- bp2.normalize.complexes(complexes)
    empty.complexes <- rep(list(integer(0)), length(complexes))
    names(empty.complexes) <- names(complexes)

    # Build node representation
    # Initialize lists
    n <- length(pathways) + length(complexes)
    proteins <- vector("list", length = n)
    sub.nodes <- vector("list", length = n)
    activated.nodes <- vector("list", length = n)
    activated.proteins <- vector("list", length = n)
    inhibited.nodes <- vector("list", length = n)
    inhibited.proteins <- vector("list", length = n)
    all.followers <- vector("list", length = n)
    all.inhibited <- vector("list", length = n)
    node.names <- c(names(pathways), names(complexes))
    node.ids <- c(bp2.objects2ids(c(pathways,complexes)))

    # Fill lists
    # Proteins are vectors of strings (that are uniprot ids)
    proteins <- c(pw.proteins, ncomplexes$proteins)
    activated.proteins <- c(act.proteins, empty.complexes)
    inhibited.proteins <- c(inh.proteins, empty.complexes)

    # Proteins that are referred to through activated.proteins or
    # inhibited.proteins but don't belong to any node are created nodes
    # of their own.
    all.proteins.in.nodes <- sort(unique(c(proteins,recursive=T)))
    all.proteins.through.controls <- sort(unique(c(activated.proteins, inhibited.proteins ,recursive=T)))
    all.proteins <- sort(unique(c(all.proteins.in.nodes, all.proteins.through.controls ,recursive=T)))
    proteins.not.in.nodes <- setdiff(all.proteins.through.controls, all.proteins.in.nodes)
    cat("Proteins not in nodes:\n ")
    print(proteins.not.in.nodes)
    
    # Nodes are indexes of other nodes in these lists
    # pw.connections.neutral and ncomplexes[[1]] contain rdf.ids. These need to be converted to indexes in node lists.
    a <- lapply(pw.connections.neutral, function(x) {match(x, node.ids)})
    b <- lapply(ncomplexes$sub.complexes, function(x) {match(x, node.ids)})
    sub.nodes <- c(a,b)
    sub.nodes <- bp2.filter.NAs.from.list.of.lists(sub.nodes)
    
    activated.nodes <- c(lapply(pw.connections.activation, function(x) {match(x, node.ids)}), empty.complexes)
    activated.nodes <- bp2.filter.NAs.from.list.of.lists(activated.nodes)

    inhibited.nodes <- c(lapply(pw.connections.inhibition, function(x) {match(x, node.ids)}), empty.complexes)
    inhibited.nodes <- bp2.filter.NAs.from.list.of.lists(inhibited.nodes)

    # Common proteins
    cat("Solving inverse proteins\n")
    inverse.proteins <- vector("list", length=length(all.proteins))
    inverse.proteins[] <- list(numeric())
    names(inverse.proteins) <- all.proteins
    for(i in 1:length(proteins))
      {
        for(j in proteins[[i]])
          {
            inverse.proteins[[j]] <- c(inverse.proteins[[j]], i)
          }
      }
    
    
    cat("Solving common proteins between nodes\n")
    nodes.with.common.proteins <- vector("list", length = n)
    nodes.with.common.proteins[] <- list(integer())
    names(nodes.with.common.proteins) <- names(node.ids)
    for(i in 1:length(inverse.proteins))
      {
        for (j in inverse.proteins[[i]])
          {
            nodes.with.common.proteins[[j]] <- unique(sort(c(nodes.with.common.proteins[[j]], setdiff(inverse.proteins[[i]], j)))) # no self references
          }
      }

    # Nodes with ...
    nodes.with.activated.proteins <- vector("list", length = n)
    nodes.with.activated.proteins[] <- list(integer())
    names(nodes.with.activated.proteins) <- names(node.ids)
    nodes.with.inhibited.proteins <- vector("list", length = n)
    nodes.with.inhibited.proteins[] <- list(integer())
    names(nodes.with.inhibited.proteins) <- names(node.ids)
    for(i in 1:n)
      {
        nodes.with.activated.proteins[[i]] <- setdiff(sort(unique(c(numeric(), inverse.proteins[ activated.proteins[[i]] ], recursive=T))),i)
        nodes.with.inhibited.proteins[[i]] <- setdiff(sort(unique(c(numeric(), inverse.proteins[ inhibited.proteins[[i]] ], recursive=T))),i)
      }
    
    # all.*
    cat("Almost ready\n")
    for(i in 1:n)
      {
        all.followers[[i]] <- sort(unique(c(sub.nodes[[i]],
                                            activated.nodes[[i]],
                                            inhibited.nodes[[i]],
                                            nodes.with.activated.proteins[[i]],
                                            nodes.with.inhibited.proteins[[i]],
                                            nodes.with.common.proteins[[i]],
                                            recursive=T)))
        all.inhibited[[i]] <- sort(unique(c(inhibited.nodes[[i]],
                                            nodes.with.inhibited.proteins[[i]],
                                            recursive=T)))
        
      }

    new("bp2.nodes",
        ids = node.ids,
        proteins = proteins,
        inverse.proteins = inverse.proteins,
        sub.nodes = sub.nodes,
        activated.nodes = activated.nodes,
        activated.proteins = activated.proteins,
        inhibited.nodes = inhibited.nodes,
        inhibited.proteins = inhibited.proteins,
        nodes.with.common.proteins = nodes.with.common.proteins,
        nodes.with.activated.proteins = nodes.with.activated.proteins,
        nodes.with.inhibited.proteins = nodes.with.inhibited.proteins,
        all.followers = all.followers,
        all.inhibited = all.inhibited)
  }
      

