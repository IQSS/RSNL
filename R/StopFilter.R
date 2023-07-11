# A simple stopword filter

### CLASSDEF

setClass("StopFilter",
  representation("FunctionalTokenFilter", stopwords="character"))

### GENERICS

setGeneric("stoplist", function (object) standardGeneric("stoplist"))

setGeneric("StopFilter",
  function (stoplist) standardGeneric("StopFilter"))

### CONSTRUCTOR

setMethod("StopFilter", "character",
  function (stoplist)  {
    fun <- function (x) tolower(x) %in% stoplist
    new("StopFilter", FunctionalTokenFilter(fun, TRUE, FALSE), 
                      stopwords=stoplist)
  })

setMethod("StopFilter", "missing",
  function (stoplist) StopFilter(stopwords("english")))

### ACCESSORS

setMethod("stoplist", "StopFilter",
  function (object) object@stopwords)


### SHOW

setMethod("show", "StopFilter",
  function (object)
    cat("A stopword filter.\n"))
