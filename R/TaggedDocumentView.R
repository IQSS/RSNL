# A view of a document as a sequence of token-tag pairs

### CLASSDEF

setClass("TaggedDocumentView",
  representation("TaggedView", "TokenizedDocumentView",
    tagFreqTable = reftype("table"),
    uniqueTags = reftype("character"),
    tokenTagFreqTable = reftype("table"),
    uniqueTokenTags = reftype("character")))

### GENERICS

setGeneric("tags", function (object) standardGeneric("tags"))

setGeneric("taggedTokens", 
  function (object) standardGeneric("taggedTokens"))

### CONSTRUCTION

# The constructor used by tag
TaggedDocumentView <- function (view, tagger, transforms=list()) {
  # TokenizedView bits
  tokens <- new("NULLcharacter")
  tokenFreqTable <- new("NULLtable")
  uniqueTokens <- new("NULLcharacter")

  # TaggedView bits
  tagFreqTable <- new("NULLtable")
  uniqueTags <- new("NULLcharacter")
  tokenTagFreqTable <- new("NULLtable")
  uniqueTokenTags <- new ("NULLcharacter")

  # XXX don't think this works with Tagged type
  # If this is a simple tagging we can take advantage of whatever
  # computation the passed-in TokenizedDV has already done, except for
  # the tokens, which we'll need to recompute with tags
  #if (length(transforms) == 0 && .keepComputed) { 
  #  tokenFreqTable <- ref(view, "tokenFreqTable")
  #  uniqueTokens <- ref(view, "uniqueTokens")
  #}

  # Update transforms
  transforms = c(tokenTransforms(view), transforms)

  # Copy as much as possible from the existing tokenized view
  res <- new("TaggedDocumentView",
             corpus=corpus(view),
             index=index(view),
             tokenizer=tokenizer(view),
             keepComputed=.keepComputed,
             transforms=transforms,
             tokens=tokens,
             tokenFreqTable=tokenFreqTable,
             uniqueTokens=uniqueTokens,
             tagger=tagger,
             tagFreqTable=tagFreqTable,
             uniqueTags=uniqueTags,
             tokenTagFreqTable=tokenTagFreqTable,
             uniqueTokenTags=uniqueTokenTags)

  registerView(corpus(view), res)
  return(res)
}

# A version of the constructor used by documentViews calls
TaggedDocumentView.corpus <- function (corpus, tagger, tokenizer, index,
                                       transforms=list()) {
  # TokenizedView bits
  tokens <- new("NULLcharacter")
  tokenFreqTable <- new("NULLtable")
  uniqueTokens <- new("NULLcharacter")

  # TaggedView bits
  tagFreqTable <- new("NULLtable")
  uniqueTags <- new("NULLcharacter")
  tokenTagFreqTable <- new("NULLtable")
  uniqueTokenTags <- new ("NULLcharacter")

  # Copy as much as possible from the existing tokenized view
  res <- new("TaggedDocumentView",
             corpus=corpus,
             index=index,
             tokenizer=tokenizer,
             keepComputed=.keepComputed,
             transforms=transforms,
             tokens=tokens,
             tokenFreqTable=tokenFreqTable,
             uniqueTokens=uniqueTokens,
             tagger=tagger,
             tagFreqTable=tagFreqTable,
             uniqueTags=uniqueTags,
             tokenTagFreqTable=tokenTagFreqTable,
             uniqueTokenTags=uniqueTokenTags)

  registerView(corpus, res)
  return(res)
}

### CALLBACK method

setMethod("clearComputed", "TaggedDocumentView",
  function (object) {
    ref(view, "tokens") <- new("NULLcharacter")
    ref(view, "tokenFreqTable") <- new("NULLtable")
    rev(view, "uniqueTokens") <- new("NULLcharacter")

    ref(object, "tagFreqTable") <- new("NULLtable")
    ref(object, "uniqueTags") <- new("NULLcharacter")
    ref(object, "tokenTagFreqTable") <- new("NULLtable")
    ref(object, "uniqueTokenTags") <- new ("NULLcharacter")
  })

# Inherits updateView from TokenizedDocumentView
### ACCESSORS

# Returns the (tagged) tokens of this TaggedDocumentView.  Note that
# this means tagging always precedes token filtering.  Overrides the
# equivalent method in TokenizedDocumentView.
# XXX assumes a character-extending document type.
setMethod("tokens", "TaggedDocumentView",
  function(object)
    if (is(ref(object, "tokens"), "NULLcharacter")) {
      tokens <- tag(document(object), tagger(object), tokenizer(object))
      for (transform in object@transforms) {
        tmp <- transformTokens(tokens, transform)
        if (is(transform, "TagSafe") | is(tmp, "Tagged"))
          tokens <- tmp
        else {
          tags <- label(tokens)
          if (length(tags) == length(tmp)) {
            warning("A transfrom of type ", as.character(class(transform)),
                    " discards tags.  Re-appending tags in order ", 
                    "(This may not be what you want if this transform ",
                    "can re-order tokens).")
            tokens <- Tagged(tmp,tags)
          } else
            warning("A transform of type ", as.character(class(transform)),
                    " discards tags and returns a token ",
                    "vector that differs in length from the input.  ",
                    "Ignoring transform!")
        }
      }
      if (keepComputed(object))
        (ref(object, "tokens") <- tokens)
      else
        tokens
    } else
      ref(object, "tokens"))

# Little helpr method for converting what
convertWhat <- function (what, options)
  if (what == "TaggedTokens") {
    return(options[1])
  } else if (what == "Tokens") {
    return(options[2])
  } else if (what == "Tags") {
    return(options[3])
  } else stop('what must be "TaggedTokens", "Tokens", or "Tags".')

# Return a frequency table of "token/tag" pairs, tokens, or tags
setMethod("freqTable", "TaggedDocumentView",
  function (object, what="TaggedTokens") {
    # Parse what
    lookup <- convertWhat(what, c("tokenTagFreqTable", "tokenFreqTable",
                                  "tagFreqTable"))

    if (is(ref(object, lookup), "nNULL")) { # lazy compute
      # Compute the appropriate thing
      result <- NULL
      if (what == "TaggedTokens")
        result <- table(taggedTokens(object))
      else if (what == "Tokens")
        result <- table(tokens(object))
      else
        result <- table(tags(object))

      # Return computed result
      if (keepComputed(object)) # save for later
        (ref(object, lookup) <- result)
      else # return and discard
        result
    } else # Load saved
      ref(object, lookup)
  })

# Returns unique "token/tag" pairs, tokens, or tags
setMethod("unique", "TaggedDocumentView", 
  function(x, incomparables="missing", what="TaggedTokens") {
    # Parse what
    lookup <- convertWhat(what, c("uniqueTokenTags", "uniqueTokens",
                                  "uniqueTags"))

    if (is(ref(x, lookup), "nNULL")) {
      result <- NULL
      if (what == "TaggedTokens") {
        result <- unique(taggedTokens(x))
       } else if (what == "Tokens") {
        result <- row.names(ref(x, "tokenFreqTable"))
        if (is.null(result))
          result <- unique(tokens(x))
       } else {
        result <- row.names(ref(x, "tagFreqTable"))
        if (is.null(result))
          result <- unique(tags(x))
       }
        
      if (keepComputed(x))
        (ref(x, lookup) <- result)
      else
        result
    } else
      ref(x, lookup)
  })

# See if object contains "token/tag" pair, token, or tag
setMethod("has", signature(object="TaggedDocumentView",
                           value="character"),
  function (object, value, what="TaggedTokens", ...)
    if (what == "TaggedTokens")
      return(value %in% taggedTokens(object))
    else if (what == "Tokens")
      return (value %in% tokens(object))
    else if (what == "Tags")
      return (value %in% tags(object))
    else stop('what must be "TaggedTokens", "Tokens", or "Tags".'))

# specializaton for Tagged
setMethod("has", signature(object="TaggedDocumentView",
                           value="Tagged"),
  function (object, value, what="TaggedTokens", ...)
    if (what == "TaggedTokens")
      has(object, flatten(value), what)
    else if (what == "Tags")
      has(object, label(value), what)
    else
      has(object, as.character(value), what))

# Return the frequency of the given "token/tag" pair, token, or tag
setMethod("freq", signature(object="TaggedDocumentView",
                            value="character"),
  function (object, value, what="TaggedTokens", ...)
    if (has(object, value, what=what))
      return (freqTable(object, what=what)[[value]])
    else
      return (0))

# specialization for Tagged
setMethod("freq", signature(object="TaggedDocumentView",
                            value="Tagged"),
  function (object, value, what="TaggedTokens", ...)
    if (what == "TaggedTokens")
      freq(object, flatten(value), what)
    else if (what == "Tags")
      freq(object, label(value), what)
    else
      freq(object, as.character(value), what))

# Returns the tags of this view
setMethod("tags", "TaggedDocumentView",
  function (object) label(tokens(object)))

# Return token-tag pairs in "token/tag" format
setMethod("taggedTokens", "TaggedDocumentView",
  function (object) flatten(tokens(object)))

### UTILITIES
setMethod("show", "TaggedDocumentView",
  function (object) {
    name <- ID(document(object))
    nTTs <- length(tokens(object))
    nUniqueTokens <- length(unique(object, what="Tokens"))
    nUniqueTags <- length(unique(object, what="Tags"))
    nUniqueTTs <- length(unique(object))

    str <- paste("A tagged document view of", name, "with", nTTs, 
                 "total tagged tokens,\n", nUniqueTTs,
                 "unique tagged tokens,", nUniqueTokens, 
                 "unique tokens, and", nUniqueTags, "unique tags\n")
    cat(str)
  }
)
