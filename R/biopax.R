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



#Package variables

.bp2 <- new.env()

.bp2[["verbose"]] <- FALSE

.bp2[["forward.slot.names"]] <-
  c("authors",
    "cellularLocation",
    "bp2.comment",
    "components",
    "controlType",
    "controlled",
    "controller",
    "dataSource",
    "db",
    "direction",
    "ecNumber",
    "featureLocation",
    "featureType",
    "id",
    "idVersion",
    "left",
    "name",
    "nextStep",
    "organism",
    "pathwayComponents",
    "physicalEntity",
    "positionStatus",
    "relationshipType",
    "right",
    "sequenceFeatureList",
    "sequenceIntervalBegin",
    "sequenceIntervalEnd",
    "sequencePosition",
    "shortName",
    "bp2.source",
    "stepInteractions",
    "stoichiometricCoefficient",
    "synonyms",
    "taxonXref",
    "term",
    "title",
    "xref",
    "year")

names(.bp2[["forward.slot.names"]]) <-
  c("http://www.biopax.org/release/biopax-level2.owl#AUTHORS",                   
    "http://www.biopax.org/release/biopax-level2.owl#CELLULAR-LOCATION",         
    "http://www.biopax.org/release/biopax-level2.owl#COMMENT",                   
    "http://www.biopax.org/release/biopax-level2.owl#COMPONENTS",                
    "http://www.biopax.org/release/biopax-level2.owl#CONTROL-TYPE",              
    "http://www.biopax.org/release/biopax-level2.owl#CONTROLLED",                
    "http://www.biopax.org/release/biopax-level2.owl#CONTROLLER",                
    "http://www.biopax.org/release/biopax-level2.owl#DATA-SOURCE",               
    "http://www.biopax.org/release/biopax-level2.owl#DB",                        
    "http://www.biopax.org/release/biopax-level2.owl#DIRECTION",                 
    "http://www.biopax.org/release/biopax-level2.owl#EC-NUMBER",                 
    "http://www.biopax.org/release/biopax-level2.owl#FEATURE-LOCATION",          
    "http://www.biopax.org/release/biopax-level2.owl#FEATURE-TYPE",              
    "http://www.biopax.org/release/biopax-level2.owl#ID",                        
    "http://www.biopax.org/release/biopax-level2.owl#ID-VERSION",                
    "http://www.biopax.org/release/biopax-level2.owl#LEFT",                      
    "http://www.biopax.org/release/biopax-level2.owl#NAME",                      
    "http://www.biopax.org/release/biopax-level2.owl#NEXT-STEP",                 
    "http://www.biopax.org/release/biopax-level2.owl#ORGANISM",                  
    "http://www.biopax.org/release/biopax-level2.owl#PATHWAY-COMPONENTS",        
    "http://www.biopax.org/release/biopax-level2.owl#PHYSICAL-ENTITY",           
    "http://www.biopax.org/release/biopax-level2.owl#POSITION-STATUS",           
    "http://www.biopax.org/release/biopax-level2.owl#RELATIONSHIP-TYPE",         
    "http://www.biopax.org/release/biopax-level2.owl#RIGHT",                     
    "http://www.biopax.org/release/biopax-level2.owl#SEQUENCE-FEATURE-LIST",     
    "http://www.biopax.org/release/biopax-level2.owl#SEQUENCE-INTERVAL-BEGIN",   
    "http://www.biopax.org/release/biopax-level2.owl#SEQUENCE-INTERVAL-END",     
    "http://www.biopax.org/release/biopax-level2.owl#SEQUENCE-POSITION",         
    "http://www.biopax.org/release/biopax-level2.owl#SHORT-NAME",                
    "http://www.biopax.org/release/biopax-level2.owl#SOURCE",                    
    "http://www.biopax.org/release/biopax-level2.owl#STEP-INTERACTIONS",         
    "http://www.biopax.org/release/biopax-level2.owl#STOICHIOMETRIC-COEFFICIENT",
    "http://www.biopax.org/release/biopax-level2.owl#SYNONYMS",                  
    "http://www.biopax.org/release/biopax-level2.owl#TAXON-XREF",                
    "http://www.biopax.org/release/biopax-level2.owl#TERM",                      
    "http://www.biopax.org/release/biopax-level2.owl#TITLE",                     
    "http://www.biopax.org/release/biopax-level2.owl#XREF",                      
    "http://www.biopax.org/release/biopax-level2.owl#YEAR")

.bp2[["inverse.slots"]] <-
  c("physicalEntity.of",
    "stepInteractions.of",
    "controlled.of",
    "nextStep.of",
    "pathwayComponents.of",
    "left.of",
    "right.of",
    "components.of",
    "controller.of",
    "sequenceFeatureList.of",
    "featureLocation.of",
    "organism.of",
    "dataSource.of",
    "cellularLocation.of",
    "featureType.of",
    "xref.of",
    "taxonXref.of",
    "sequenceBegin.of",
    "sequenceEnd.of")

names(.bp2[["inverse.slots"]]) <-
  c("physicalEntity",
    "stepInteractions",
    "controlled",
    "nextStep",
    "pathwayComponents",
    "left",
    "right",
    "components",
    "controller",
    "sequenceFeatureList",
    "featureLocation",
    "organism",
    "dataSource",
    "cellularLocation",
    "featureType",
    "xref",
    "taxonXref",
    "sequenceBegin",
    "sequenceEnd")

.bp2[["all.slot.names"]] <- c(.bp2[["forward.slot.names"]], .bp2[["inverse.slots"]])
names(.bp2[["all.slot.names"]]) <- NULL

setClass("bp2.rdf.entity",
         representation(rdf.id = "integer",
                        "VIRTUAL"))

setClass("bp2.entity",
         representation(availability = "integer",
                        name = "integer",
                        shortName = "integer",
                        bp2.comment = "integer",
                        dataSource = "integer",
                        xref = "vector",
                        synonyms = "vector",
                        "VIRTUAL"),
         contains = "bp2.rdf.entity")

setClass("bp2.pathway",
         representation(organism = "integer",
                        evidence = "integer",
                        pathwayComponents = "vector",
                        stepInteractions.of = "vector",
                        controlled.of = "vector"),
         contains = "bp2.entity")

setClass("bp2.interaction",
         representation(participants = "vector",
                        evidence = "integer"),
         contains = "bp2.entity")

setClass("bp2.physicalEntity",
         representation(physicalEntity.of = "vector"),
         contains = "bp2.entity")

setClass("bp2.physicalInteraction",
         representation(interaction.type = "integer",
                        controlled.of="vector",
                        stepInteractions.of = "vector"),
         contains = "bp2.interaction")

setClass("bp2.control",
         representation(controlType = "integer",
                        controller ="vector",
                        controlled = "vector"),
         contains = "bp2.physicalInteraction")

setClass("bp2.conversion",
         representation(spontaneous = "integer",
                        left = "vector",
                        right = "vector"),
         contains = "bp2.physicalInteraction")

setClass("bp2.catalysis",
         representation(direction = "integer",
                        cofactors = "vector"),
         contains = "bp2.control")

setClass("bp2.modulation",
         contains = "bp2.control")

setClass("bp2.biochemicalReaction",
         representation(deltaG = "integer",
                        deltaH = "integer",
                        ecNumber = "integer",
                        keq = "integer",
                        deltaS = "integer"),
         contains = "bp2.conversion")

setClass("bp2.complexAssembly",
         contains = "bp2.conversion")

setClass("bp2.transport",
         contains = "bp2.conversion")

setClass("bp2.transportWithBiochemicalReaction",
         contains = "bp2.biochemicalReaction")

setClass("bp2.dna",
         representation(sequence = "integer",
                        organism = "integer"),
         contains = "bp2.physicalEntity")

setClass("bp2.rna",
         representation(sequence = "integer",
                        organism = "integer"),
         contains = "bp2.physicalEntity")

setClass("bp2.protein",
         representation(sequence = "integer",
                        organism = "integer"),
         contains = "bp2.physicalEntity")

setClass("bp2.smallMolecule",
         representation(chemicalFormula = "integer",
                        structure = "integer",
                        molecularWeight= "integer"),
         contains = "bp2.physicalEntity")

setClass("bp2.complex",
         representation(components = "vector",
                        organism = "integer"),
         contains = "bp2.physicalEntity")

setClass("bp2.utilityClass",
         representation(bp2.comment = "integer",
                        "VIRTUAL"),
         contains = "bp2.rdf.entity")

setClass("bp2.chemicalStructure",
         representation(structureFormat = "integer",
                        structureData = "integer"),
         contains = "bp2.utilityClass")

setClass("bp2.confidence",
         representation(confidenceValue = "integer",
                        xref = "vector"),
         contains = "bp2.utilityClass")

setClass("bp2.evidence",
         representation(xref = "vector",
                        experimentaForm = "integer",
                        evidenceCode = "integer",
                        confidence = "integer"),
         contains = "bp2.utilityClass")

setClass("bp2.experimentalForm",
         representation(participant = "integer",
                        experimentaFormType = "integer"),
         contains = "bp2.utilityClass")

setClass("bp2.externalReferenceUtilityClass",
         contains = "bp2.utilityClass")

setClass("bp2.pathwayStep",
         representation(stepInteractions = "vector",
                        nextStep = "integer",
                        nextStep.of = "vector",
                        pathwayComponents.of = "vector"),
         contains = "bp2.utilityClass")

setClass("bp2.physicalEntityParticipant",
         representation(physicalEntity = "integer",
                        stoichiometricCoefficient = "integer",
                        cellularLocation = "integer",
                        left.of = "vector",
                        right.of = "vector",
                        components.of = "vector",
                        controller.of = "vector"),
         contains = "bp2.utilityClass")

setClass("bp2.sequenceFeature",
         representation(name = "integer",
                        featureType = "integer",
                        shortName = "integer",
                        synonyms = "integer",
                        xref = "integer",
                        featureLocation = "integer",
                        sequenceFeatureList.of = "vector"),
         contains = "bp2.utilityClass")

setClass("bp2.sequenceLocation",
         representation(featureLocation.of = "vector"),
         contains = "bp2.utilityClass")

setClass("bp2.bioSource",
         representation(celltype = "integer",
                        tissue = "integer",
                        name = "integer",
                        taxonXref = "integer",
                        organism.of = "vector"),
         contains = "bp2.externalReferenceUtilityClass")

setClass("bp2.dataSource",
         representation(name = "integer",
                        xref = "integer",
                        dataSource.of = "vector"),
         contains = "bp2.externalReferenceUtilityClass")

setClass("bp2.openControlledVocabulary",
         representation(term = "integer",
                        xref = "integer",
                        cellularLocation.of = "vector",
                        featureType.of = "vector"),
         contains = "bp2.externalReferenceUtilityClass")

setClass("bp2.xref",
         representation(db = "integer",
                        dbVersion = "integer",
                        id = "integer",
                        idVersion = "integer",
                        xref.of = "vector"),
         contains = "bp2.externalReferenceUtilityClass")

setClass("bp2.publicationXref",
         representation(title = "integer",
                        year = "integer",
                        authors = "integer",
                        url = "integer",
                        bp2.source = "integer"),
         contains = "bp2.xref")

setClass("bp2.relationshipXref",
         representation(relationshipType = "integer"),
         contains = "bp2.xref")

setClass("bp2.unificationXref",
         representation(taxonXref.of = "vector"),
         contains = "bp2.xref")

setClass("bp2.sequenceParticipant",
         representation(sequenceFeatureList = "vector"),
         contains = "bp2.physicalEntityParticipant")

setClass("bp2.sequenceInterval",
         representation(sequenceIntervalEnd = "integer",
                        sequenceIntervalBegin = "integer"),
         contains = "bp2.sequenceLocation")

setClass("bp2.sequenceSite",
         representation(positionStatus = "integer",
                        sequencePosition = "integer",
                        sequenceBegin.of = "vector",
                        sequenceEnd.of = "vector"),
         contains = "bp2.sequenceLocation")

setClass("bp2.model",
         representation(all.objects="list",
                        symbol.names="vector",
                        predicate.names="vector",
                        all.slot.names="vector"))

bp2.simple.show <- function(object)
{
  cat(paste("Object of class \"", class(object),
            "\" with slots: ", paste(slotNames(object), collapse="\n"),
            "\n", sep=""))
  invisible(object)
}

setMethod("show", "bp2.model", bp2.simple.show);

#internal helper functions

.bp2.createPredicateNameSlotMapping <- function()
{
  mapping <- vector("character",0)
  for(i in 1:length(.bp2[["forward.slot.names"]]))
    {
      if(names(.bp2[["forward.slot.names"]])[i] %in% .bp2[["predicate.names"]])
      {
        mapping[match(names(.bp2[["forward.slot.names"]])[i], .bp2[["predicate.names"]])] <- .bp2[["forward.slot.names"]][i]
      }
    }
  .bp2[["predicate.mapping"]] <- mapping
}

setMethod("initialize", "bp2.rdf.entity", function(.Object, id) {
  .Object@rdf.id <- id
  return(.Object)
})

bp2.rdf.entity.print <- function(object)
{
  cat(paste("Object of class", class(object), "\n", .bp2[["symbol.names"]][slot(object,"rdf.id")], "\n"))
  for(i in slotNames(object))
    {
      cat(paste("    ", i, "=>", .bp2[["symbol.names"]][slot(object,i)], "\n"))
    }
  invisible(object)
}

setMethod("show", "bp2.rdf.entity", bp2.rdf.entity.print);

.bp2.get.class <- function(typeName)
{
  if(length(grep("http://www.biopax.org/release/biopax-level2.owl#",typeName)) == 1)
  {
    return(paste("bp2.",(sub(".*#", "", typeName)),sep=""))
    }
  return(NA)  
}

bp2.simplifySymbolName <- function(longName)
{
  return(sub("http://.*#", "", longName))
}

.bp2.add.forward.relations <- function()
{
  cat("Setting forward relations between objects\n")
  dat <- .bp2[["object.data"]]
  obj <- .bp2[["all.objects"]]
  pre <- .bp2[["predicate.mapping"]]
  for(i in 1:length(dat$subject))
    {
      if (i%%10000 == 0)
        {
          if (.bp2[["verbose"]])
            {
              cat(paste(date(), ":", i, "/", length(dat$subject),"\n"))
            }
        }
      if (!is.null(obj[i]) && !is.na(pre[dat[i,2]]))
        {
          #print(paste("predicate id", dat[i,2], "slot for predicate", pre[dat[i,2]]))
          new.value <- c(slot(obj[[dat[i,1]]], pre[dat[i,2]]), dat[i,3])
          slot(obj[[dat[i,1]]], pre[dat[i,2]]) <- new.value
        }
    }
  .bp2[["all.objects"]] <- obj
}

.bp2.add.inverse.relations <- function()
{
  cat("Setting inverse relations between objects\n")
  objs <- .bp2[["all.objects"]]
  inv <- .bp2[["inverse.slots"]]
  for(i in 1:length(.bp2[["all.objects"]]))
    {
      if (i%%1000 == 0)
        {
          if (.bp2[["verbose"]])
            {
              cat(paste(date(), ":", i, "/", length(objs),"\n"))
            }
        }
      obj <- objs[[i]]
      obj.slots <- slotNames(obj)[slotNames(obj) %in% names(inv)]
      for(s in obj.slots)
        {
          for (value in slot(obj, s))
            {
              new.value <- c(slot(objs[[value]],inv[s]),i)
              slot(objs[[value]], inv[s]) <- new.value
            }
        }
    }
    .bp2[["all.objects"]] <- objs
}

.bp2.sequence <- function(a, b) {if (a <= b) a:b else numeric(0)}

### Pubic API starts here

# Reads biopax data in RDF format to data frame using Rredland package
# The data.frame has three columns: subject, predicate,
# object. Subjects are rdf ids for biopax objects. Predicates are the
# slots of the biopax objects and objects are the values of the slots
# that can be either string/int/etc values or references to other rdf
# ids.
# The data frame is converted to object model with bp2.create.graph function
bp2.read.uri <- function(uri)
{
  biopax_data_location <- makeRedlURI(uri)
  biopax_data <- readRDF(biopax_data_location, storageType="bdb")
  return(as(biopax_data, "data.frame"))
}

# Creates Biopax object graph from RDF data frame that contains defintion of a biopax model.
bp2.create.model <- function(bp)
  {
    # These two tables contain character -> numeric mapping of names and predicates
    .bp2[["symbol.names"]] <- unique(c(levels(bp$subject),levels(bp$object)))
    .bp2[["predicate.names"]] <- levels(bp$predicate)

    #s ymbol names is a factor of all strings that appear as subject OR
    # object names all subjects and objects will be presented as
    # integers that use these levels

    .bp2.createPredicateNameSlotMapping()
    classNames <- as.character(sapply(.bp2[["symbol.names"]],.bp2.get.class))

    # create new data.frame where subject and object are factors with same set of levels.
    new.bp <- data.frame(subject=as.integer(factor(bp$subject, levels=.bp2[["symbol.names"]])),
                         predicate=as.integer(bp$predicate),
                         object=as.integer(factor(bp$object, levels=.bp2[["symbol.names"]])))
    .bp2[["object.data"]] <- new.bp 
    elements <- new.bp[(new.bp$predicate == match("http://www.w3.org/1999/02/22-rdf-syntax-ns#type", .bp2[["predicate.names"]])),]
    num_elements <- length(elements$subject)
    all.objects <- vector(mode="list", num_elements)
    names(all.objects) <- .bp2[["symbol.names"]][1:num_elements]
    
    #iterate through object data
    cat("Allocating objects\n")
    for (i in 1:num_elements)
      {
        className <- classNames[elements[i,3]]
        subjectId <- elements[i,1]
        if (i%%1000 == 0)
          {
            if (.bp2[["verbose"]])
              {
                cat(paste(date(), ":", i, "/", num_elements,"\n"))
              }
          }
        if(!is.na(className))
          {
            all.objects[subjectId] <- new(className, id=subjectId)
          }
      }
    .bp2[["all.objects"]] <- all.objects
    .bp2.add.forward.relations()
    .bp2.add.inverse.relations()
    return(new("bp2.model", all.objects=.bp2[["all.objects"]], symbol.names=.bp2[["symbol.names"]], predicate.names=.bp2[["predicate.names"]], all.slot.names=.bp2[["all.slot.names"]]))
  }


# Return Biopax S4 objects for given vector of numeric object ids
bp2.ids2objects <- function(id)
{
  if (is.numeric(id))
    {
      return(.bp2[["all.objects"]][id])
    }
  else
    {
      stop("Argument must be numeric")
    }
}

# Return numeric IDs of given objects in bp2.all.objects list
bp2.objects2ids <- function(objs)
  {
    if (is.list(objs))
      {
        sapply(objs, function(x) { slot(x,'rdf.id') })
      }
    else if (inherits(objs,'bp2.rdf.entity'))
      {
        slot(objs,'rdf.id')
      }
    else
      {
        stop("Agument must be instance of bp2.rdf.entity or list of such objects")
      }
  }

# Return numeric IDs of given objects in bp2.all.objects list
bp2.ids2names <- function(id)
{
  if (is.numeric(id))
    {
      return(names(.bp2[["all.objects"]])[id])
    }
  else
    {
      stop("Argument must be numeric")
    }
}

bp2.exists <- function(id)
{
  return(!(sapply(.bp2[["all.objects"]][id], is.null)))
}

bp2.get.all.class.names <- function()
  {
    classes <- unique(sapply(.bp2[["all.objects"]], class))
    classes <- classes[!(classes == "NULL")]
    return(classes)
  }

bp2.get.all.slot.names <- function()
  {
    return(unique(c(lapply(bp2.get.all.class.names(),slotNames), recursive=T)))
  }

bp2.verbose <- function(new.value=NULL)
{
  old.value <- .bp2[["verbose"]]
  if (!is.null(new.value))
    {
      .bp2[["verbose"]] <- new.value
    }
  invisible(old.value)
}

bp2.resume <- function(bp2.model)
{
  .bp2[["all.objects"]] <- bp2.model@all.objects
  .bp2[["symbol.names"]] <- bp2.model@symbol.names
  .bp2[["predicate.names"]] <- bp2.model@predicate.names
  .bp2[["all.slot.names"]] <- bp2.model@all.slot.names
}

