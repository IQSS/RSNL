# A simple regular expression document filter

### CLASSDEF

setClass("RegexDocumentFilter",
  representation("FunctionalDocumentFilter", pattern="Pattern"))

### GENERICS

setGeneric("RegexDocumentFilter",
  function (pattern, negate=FALSE) standardGeneric("RegexDocumentFilter"))

### CONSTRUCTOR

setMethod("RegexDocumentFilter", "Pattern",
  function (pattern, negate=FALSE) {
    fun <- function (x) re.find(re.matcher(pattern, document(x)))
    new("RegexDocumentFilter", FunctionalDocumentFilter(fun, negate, TRUE),
                            pattern=pattern)
  })

setMethod("RegexDocumentFilter", "character",
  function (pattern, negate=FALSE)
    RegexDocumentFilter(re.compile(pattern), negate))

### ACCESSORS

setMethod("pattern", "RegexDocumentFilter",
  function (object) object@pattern)

### SHOW

setMethod("show", "RegexDocumentFilter",
  function (object)
    if (isNegated(object))
      cat("A negated regular expression document filter using pattern:\n",
          re.pattern(pattern(object)), "\n")
    else
      cat("A regular expression document filter using pattern:\n",
          re.pattern(pattern(object)), "\n"))
