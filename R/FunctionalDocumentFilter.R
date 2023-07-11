# A simple functor object for encapsulating document filter functions.
# The encapsulated function should expect a list of DocumentView
# objects unless applyToEach is set, in which case it should expect a
# single DocumentView object.  In the first case it should return a
# list of logicals, each TRUE indicating a docview to keep and each
# FALSE indicating one to filter.  In the second case it should return
# TRUE (if the object should be kept) or FALSE (if it should be
# filtered out of the final view).

### CLASSDEF

setClass("FunctionalDocumentFilter",
  representation("DocumentFilter", fun="function", negate="logical",
                                   applyToEach="logical"))

### GENERICS

setGeneric("FunctionalDocumentFilter",
  function(fun, negate, applyToEach)
    standardGeneric("FunctionalDocumentFilter"))

setGeneric("func", function (object) standardGeneric("func"))

setGeneric("isNegated",  function (object) standardGeneric("isNegated"))

setGeneric("isAppliedToEach",
  function (object) standardGeneric("isAppliedToEach"))


### CONSTRUCTOR

setMethod("FunctionalDocumentFilter", signature(fun="function", 
                                             negate="logical",
                                             applyToEach="logical"),
  function (fun, negate, applyToEach)
    new("FunctionalDocumentFilter", fun=fun, negate=negate,
                                    applyToEach=applyToEach))

setMethod("FunctionalDocumentFilter", signature(fun="function",
                                             negate="missing",
                                             applyToEach="logical"),
  function (fun, negate, applyToEach)
    FunctionalDocumentFilter(fun, FALSE, applyToEach))

setMethod("FunctionalDocumentFilter", signature(fun="function",
                                             negate="missing",
                                             applyToEach="missing"),
  function (fun, negate, applyToEach)
    FunctionalDocumentFilter(fun, FALSE, FALSE))

setMethod("FunctionalDocumentFilter", signature(fun="function",
                                             negate="logical",
                                             applyToEach="missing"),
  function (fun, negate, applyToEach)
    FunctionalDocumentFilter(fun, negate, FALSE))

### ACCESSORS

setMethod("func", "FunctionalDocumentFilter",
  function (object) object@fun)

setMethod("isNegated", "FunctionalDocumentFilter",
  function (object) object@negate)

setMethod("isAppliedToEach", "FunctionalDocumentFilter",
  function (object) object@applyToEach)

### FILTER overriding

# Filter a list of DocumentView objects
setMethod("filterDocuments", signature(object="list", 
                                       filter="FunctionalDocumentFilter"),
  function(object, filter) {
    # Set up the function
    fun <- func(filter)
    if (isNegated(filter))
      fn <- function (x) ! fun(x)
    else
      fn <- fun

    # Extract the document views and filter
    if (isAppliedToEach(filter))
      object[sapply(object, fn)]
    else
      object <- object[ fn(object) ]
  })
 
### SHOW

setMethod("show", "FunctionalDocumentFilter",
  function (object)
    if (isNegated(object))
      cat("A negated functional document filter using function:\n",
          format(func(object)), "\n")
    else
      cat("A functional document filter using function:\n",
          format(func(object)), "\n"))
