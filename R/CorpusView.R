### FORWARD REF
setClass("DocumentFilter", representation("VIRTUAL"))

### CLASSDEF

# corpusView - A virtual class that forms the base of classes used to 
# view the content of a collection of documents in a new way.
setClass("CorpusView",
    representation(
        "VIRTUAL",
        "View",
        documentViews = reftype("list"),
        keepDocumentViews = "logical",
        documentFilters = "list"
    )
)

### GENERICS
setGeneric("documentViews",
  function (object) standardGeneric("documentViews"))

setGeneric("documentFilters",
  function (object) standardGeneric("documentFilters"))

setGeneric("keepDocumentViews",
  function (object) standardGeneric("keepDocumentViews"))

setGeneric("keepDocumentViews<-",
  function (object, value) standardGeneric("keepDocumentViews<-"))

### ACCESSORS

# Returns the length of the view.  This can be shorter than the
# length of the underlying corpus if a filter is applied.
setMethod("length", "CorpusView", 
  function(x) return(length(documentViews(x))))

# Returns the documentViews underlying the corpusview
# This needs to be redefined by extending classes
# Would normally leave this undefined, but makes interface clear
setMethod("documentViews", "CorpusView", 
  function(object)
    stop("documentViews must be implemented by extending classes"))

# Return a documentview
setMethod("[[", signature(x = "CorpusView", j = "missing"),
  function (x, i, j) documentViews(x)[[i]])

# Return a slice of doucment views
setMethod("[", signature(x = "CorpusView", j = "missing",drop = "missing"),
  function (x, i, j, drop) documentViews(x)[i])

# See if this corpus view automatically drops documentviews
setMethod("keepDocumentViews", "CorpusView",
  function (object) object@keepDocumentViews)

# See all of this cv's filters
setMethod("documentFilters", "CorpusView",
  function (object) object@documentFilters)

### MODIFIERS

# Direct this corpus view to keep or discard document views btwn calls
setMethod("keepDocumentViews<-", signature("CorpusView", "logical"),
  function(object, value) {
    if (! value)
      ref(object, "documentViews") <- new("NULLlist")
    object@keepDocumentViews <- value
    object
  })

# Override this to propagate status to documentviews correctly.
# Basically we just dump all the document views if necessary and they
# get re-constructed with the appropriate keepComputed value on the
# next call to the documentViews method.
setMethod("keepComputed<-", signature("CorpusView", "logical"),
  function (object, value) {
    if (value != keepComputed(object))
      ref(object, "documentViews") <- new("NULLlist")
    if (! value)
      clearComputed(object)
    object@keepComputed <- value
    object
  })
