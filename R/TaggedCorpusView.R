
### CLASSDEF

setClass("TaggedCorpusView",
  representation("TaggedView", "TokenizedCorpusView",
                 uniqueTags = reftype("character"),
                 uniqueTokenTags = reftype("character"),
                 tagFreqTable = reftype("table"),
                 tokenTagFreqTable = reftype("table"),
                 documentTagMatrix = reftype("DocumentTermMatrix"),
                 documentTokenTagMatrix = reftype("DocumentTermMatrix")))

### GENERICS

setGeneric("documentTaggedTokenMatrix",
  function (object, weight) standardGeneric("documentTaggedTokenMatrix"))
setGeneric("documentTagMatrix",
  function (object, weight) standardGeneric("documentTagMatrix"))
### CONSTRUCTION

TaggedCorpusView <- function (view, tagger, transforms=list(),
                              documentFilters=list()) {
  # Tokenized bits
  documentViews <- new("NULLlist")
  tokenFreqTable <- new("NULLtable")
  uniqueTokens <- new("NULLcharacter")
  documentTokenMatrix <- new("NULLDocumentTermMatrix")

  # Tagged bits
  uniqueTags <- new("NULLcharacter")
  uniqueTokenTags <- new("NULLcharacter")
  tagFreqTable <- new("NULLtable")
  tokenTagFreqTable <- new("NULLtable")
  documentTagMatrix <- new("NULLDocumentTermMatrix")
  documentTokenTagMatrix <- new("NULLDocumentTermMatrix")

  # XXX for now, don't bother with this because it seems like a
  # sub-optimal approach in most cases, unlike the dv-level situation.
  # If this is a simple tagging we can save some computation
  #if (length(transforms) == 0 && length(documentFilters) == 0 &&
  #    .keepComputed) {
  #  uniqueTokens <- ref(view, "uniqueTokens")
  #  tokenFreqTable <- ref(view, "tokenFreqTable")
  #  documentTokenMatrix <- ref(view, "documentTokenMatrix")
  #}

  # Update transforms and doc-filters
  transforms = c(tokenTransforms(view), transforms)
  documentFilters = c(documentFilters(view), documentFilters)

  res <- new("TaggedCorpusView",
             corpus=corpus(view),
             keepComputed=.keepComputed,
             tokenizer=tokenizer(view),
             transforms=transforms,
             documentFilters=documentFilters,
             documentViews=documentViews,
             keepDocumentViews=.keepDocumentViews,
             tokenFreqTable=tokenFreqTable,
             uniqueTokens=uniqueTokens,
             documentTokenMatrix=documentTokenMatrix,
             tagger=tagger,
             uniqueTags=uniqueTags,
             uniqueTokenTags=uniqueTokenTags,
             tagFreqTable=tagFreqTable,
             tokenTagFreqTable=tokenTagFreqTable,
             documentTagMatrix=documentTagMatrix,
             documentTokenTagMatrix=documentTokenTagMatrix)

  registerView(corpus(view), res)
  return(res)
}

### CALLBACK methods

setMethod("clearComputed", "TaggedCorpusView",
  function (object) {
    ref(object, "tokenFreqTable") <- new("NULLtable")
    ref(object, "uniqueTokens") <- new("NULLcharacter")
    ref(object, "documentTokenMatrix") <- new("NULLDocumentTermMatrix")

    ref(object, "uniqueTags") <- new("NULLcharacter")
    ref(object, "uniqueTokenTags") <- new("NULLcharacter")
    ref(object, "tagFreqTable") <- new("NULLtable")
    ref(object, "tokenTagFreqTable") <- new("NULLtable")
    ref(object, "documentTagMatrix") <- new("NULLDocumentTermMatrix")
    ref(object, "documentTokenTagMatrix") <- new("NULLDocumentTermMatrix")
  })

# inherits updateView

### ACCESSORS

setMethod("documentViews", "TaggedCorpusView",
  function (object) {
    if (is(ref(object, "documentViews"), "NULLlist")) {
      proto <- TaggedDocumentView.corpus(corpus(object), tagger(object),
        tokenizer(object), 1, object@transforms)

      genTokDVs(object, proto) # handles doc filters, etc
    } else
      ref(object, "documentViews")
  })

setMethod("freqTable", "TaggedCorpusView",
  function(object, what="TaggedTokens") {
    lookup <- convertWhat(what, c("tokenTagFreqTable", "tokenFreqTable",
                                  "tagFreqTable"))

    if (is(ref(object, lookup), "nNULL")) {
      result <- NULL
      if (what == "TaggedTokens") # XXX check this
        result <- table(unlist(lapply(documentViews(object), taggedTokens),
                        use.names=F))
      else if (what == "Tokens")
        result <- table(unlist(lapply(documentViews(object), tokens),
                        use.names=F))
      else
        result <- table(unlist(lapply(documentViews(object), tags),
                        use.names=F))

      if (keepComputed(object))
        (ref(object, lookup) <- result)
      else
        result
   } else
    ref(object, lookup)
 })

setMethod("unique", "TaggedCorpusView",
  function (x, incomparables="missing", what="TaggedTokens") {
    lookup <- convertWhat(what, c("uniqueTokenTags", "uniqueTokens",
                                  "uniqueTags"))
    if (is(ref(x, lookup), "nNULL")) {
        result <- unique(unlist(lapply(documentViews(x), unique, what=what)))
      if (keepComputed(x))
        (ref(x, lookup) <- result)
      else
        result
   } else
    ref(x, lookup)
 })

# See if object contains "token/tag" pair, token, or tag
setMethod("has", signature(object="TaggedCorpusView",
                           value="character"),
  function (object, value, what="TaggedTokens", ...)
    return (value %in% unique(object, what=what)))

# Specialization for Tagged
setMethod("has", signature(object="TaggedCorpusView",
                           value="Tagged"),
  function (object, value, what="TaggedTokens", ...)
    if (what == "TaggedTokens")
      has(object, flatten(value), what)
    else if (what == "Tags")
      has(object, label(value), what)
    else
      has(object, as.character(value), what))

# Return the frequency of the given "token/tag" pair, token, or tag
setMethod("freq", signature(object="TaggedCorpusView",
                            value="character"),
  function (object, value, what="TaggedTokens", ...)
    if (has(object, value, what=what))
      return (freqTable(object, what=what)[[value]])
    else
      return (0))

# Specialization for Tagged
setMethod("freq", signature(object="TaggedCorpusView",
                            value="Tagged"),
  function (object, value, what="TaggedTokens", ...)
    if (what == "TaggedTokens")
      freq(object, flatten(value), what)
    else if (what == "Tags")
      freq(object, label(value), what)
    else
      freq(object, as.character(value), what))

# Inherits documentTokenMatrix from TokenizedCorpusView

# Return the document-token/tag matrix, optionally weighted
setMethod("documentTaggedTokenMatrix", 
          signature(object="TaggedCorpusView",
                    weight="missing"),
  function (object, weight=weightTf)
    if (is(ref(object, "documentTokenTagMatrix"), "NULLDocumentTermMatrix")) {
      result <- makeDocumentTokenMatrix(lapply(documentViews(object),
                                           freqTable, what="TaggedTokens"),
                                        unique(object, what="TaggedTokens"),
                                        names(documentViews(object)))
      if (keepComputed(object))
        (ref(object, "documentTokenTagMatrix") <- result)
      else
        result
    } else
      ref(object, "documentTokenTagMatrix"))
 
setMethod("documentTaggedTokenMatrix", 
          signature(object="TaggedCorpusView",
                    weight="WeightFunction"),
  function (object, weight=weightTf)
    weight(documentTaggedTokenMatrix(object)))

# Return the document-tag matrix, optionally weighted
setMethod("documentTagMatrix", 
          signature(object="TaggedCorpusView",
                    weight="missing"),
  function (object, weight=weightTf)
    if (is(ref(object, "documentTagMatrix"), "NULLDocumentTermMatrix")) {
      result <- makeDocumentTokenMatrix(lapply(documentViews(object),
                                               freqTable, what="Tags"),
                                        unique(object, what="Tags"),
                                        names(documentViews(object)))
      if (keepComputed(object))
        (ref(object, "documentTagMatrix") <- result)
      else
        result
    } else
      ref(object, "documentTagMatrix"))
 
setMethod("documentTagMatrix", 
          signature(object="TaggedCorpusView",
                    weight="WeightFunction"),
  function (object, weight=weightTf)
    weight(documentTagMatrix(object)))

### UTILITIES

setMethod("show", "TaggedCorpusView",
  function (object) {
    nUniqueTok <- length(unique(object, what="Tokens"))
    nUniqueTag <- length(unique(object, what="Tags"))
    nUniqueTT <- length(unique(object, what="TaggedTokens"))
    nTotalTT <- sum(documentTaggedTokenMatrix(object)$v)

    str <- paste("A tagged corpus view with", nTotalTT,
                 "total tagged tokens,\n", nUniqueTT, 
                 "unique tagged tokens,", nUniqueTok, 
                 "unique tokens, and", nUniqueTag, "unique tags\n")
    cat(str)
  }
)
