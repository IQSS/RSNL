# A base class for views of RSNLCorpus objects

### CLASSDEF

setClass("View",
  representation("VIRTUAL",
                 corpus="RSNLCorpus",
                 keepComputed = "logical"))

### GENERICS

setGeneric("corpus", function (object) standardGeneric("corpus"))

setGeneric("keepComputed",
  function (object) standardGeneric("keepComputed"))

setGeneric("keepComputed<-",
  function (object, value) standardGeneric("keepComputed<-"))

setGeneric("clearComputed",
  function (object) standardGeneric("clearComputed"))

### ACCESSORS

# Get the corpus viewed
setMethod("corpus", "View",
  function (object) object@corpus)

# Does this object keep computed objects or discard them each time?
setMethod("keepComputed", "View",
  function (object) object@keepComputed)

### MODIFIERS

# Set the computed status for this object
setMethod("keepComputed<-", signature("View", "logical"),
  function (object, value) {
    if (! value)
      clearComputed(object)
    object@keepComputed <- value
    object
  })
