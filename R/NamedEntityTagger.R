### CLASS DEF

setClass("NamedEntityTagger",
  representation("Tagger",
                 ref="jobjRef",
                 model="character"))
 
### GENERICS
setGeneric("model", function (object) standardGeneric("model"))

### CONSTRUCTION

NamedEntityTagger <- function (model="3-class", distsim=FALSE) { 
  filename <- NULL
  if (model == "3-class")
    filename <- "taggers/ner/ner-eng-ie.crf-3-all2008"
  else if (model == "4-class")
    filename <- "taggers/ner/ner-eng-ie.crf-4-conll"
  else
    stop("NER Tagger model must be '3-class' or '4-class'")

  if (distsim) {
    filename <- paste(filename, "distsim", sep="-")
    model <- paste(model, "distsim", sep="-")
  }
  
  filename <- system.file(paste(filename, "ser.gz", sep="."),
                          package="RSNL")

  new("NamedEntityTagger", ref=.jnew("NamedEntityTaggerBridge", filename),
      model=model)
}

### ACCESSORS
setMethod("model", "NamedEntityTagger",
  function (object) object@model)

### tag overriding

# Works just like MaxentTagger.  See for details
setMethod("tag", signature(object="character",
                           tagger="NamedEntityTagger",
                           tokenizer="missing",
                           index="missing"),
  function(object, tagger, tokenizer, index) {
    if (is(object, "TextDocument"))
      object <- paste(object, collapse=" ")
    if (length(object) == 1)
      .jcall(tagger@ref, "V", "tagString", object)
    else
      .jcall(tagger@ref, "V", "tagTokens", object)

    tokens <- .jcall(tagger@ref, "[S", "getTokens")
    tags <- .jcall(tagger@ref, "[S", "getTags")

    Tagged(tokens, tags)
  })

# Explicitly override this to tokenize in place
setMethod("tag", signature(object="character", tagger="NamedEntityTagger",
                           tokenizer="PTBTokenizer", index="missing"),
  function(object, tagger, tokenizer, index) {
    if (length(object) > 1)
      object <- paste(object, collapse=" ")

    .jcall(tagger@ref, "V", "tagString", object)
    tokens <- .jcall(tagger@ref, "[S", "getTokens")
    tags <- .jcall(tagger@ref, "[S", "getTags")

    Tagged(tokens, tags)
  })

### SHOW

setMethod("show", "NamedEntityTagger",
  function (object)
    cat("A Named Entity Tagger using model ", model(object), ".\n"))
