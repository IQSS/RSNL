# A filter that filters out tokens from objects based on the number of
# times the tokens appear in a given tokenized corpus view.  Note that
# one doesn't have to apply the filter to documents from the corpus it
# was built from, but one probably should.

### CLASSDEF

setClass("TokenDocFreqFilter",
  representation("FunctionalTokenFilter", min="numeric", max="numeric"))

### GENERICS

setGeneric("TokenDocFreqFilter",
  function (view, min=1, max=Inf)
    standardGeneric("TokenDocFreqFilter"))

setGeneric("minFreq",
  function (object) standardGeneric("minFreq")) 

setGeneric("maxFreq",
  function (object) standardGeneric("maxFreq")) 

### CONSTRUCTOR

# If min and max are less than one (>=0) they are interpreted as
# fractions of the total number of documents.  If they are greater or
# equal to one they are interpreted as numbers of documents.

setMethod("TokenDocFreqFilter", "TokenizedCorpusView",
  function (view, min=1, max=Inf)  {
    if (! is.numeric(min) || length(min) > 1 || min < 0)
      stop(paste("Bad min value:", min))
    if (! is.numeric(max) || length(max) > 1 || max < 0)
      stop(paste("Bad max value:", max))
    if (max < min)
      stop(paste("Max and min do not conform: (", min, ",", max, ")"))

    # Set up percentage case
    nDocs <- length(corpus(view))
    if (min < 1) {
      min <- nDocs * min
      max <- nDocs * max
    }

    # This approach was very slow, so we use a less flexible setup for
    # now.
    # We want our own view, not a reference to the original, just in
    # case.
    #copy <- clone(view)
    #keepComputed(copy) <- TRUE # Dumping comps is just stupid for this

    # Close view.  Note that this filter will update with the view...
    #fun <- function (x) {
    #  vec <- apply(documentTokenMatrix(copy, weightBin), 2, sum)[x]
    # return (min <= vec & vec <= max)
    #}

    vec <- apply(documentTokenMatrix(view, weightBin), 2, sum)
    fun <- function (x) {
      vec.final <- vec[x]
      return (min <= vec.final & vec.final <= max)
    }

    new("TokenDocFreqFilter", FunctionalTokenFilter(fun, FALSE, FALSE),
        min=min, max=max)
  })

### ACCESSORS

setMethod("minFreq", "TokenDocFreqFilter",
  function (object) object@min)

setMethod("maxFreq", "TokenDocFreqFilter",
  function (object) object@max)

### SHOW

setMethod("show", "TokenDocFreqFilter",
  function (object)
    cat("A token-document frequency filter with minimum frequency ",
        minFreq(object), " and maximum frequency ", maxFreq(object),
        ".\n"))
