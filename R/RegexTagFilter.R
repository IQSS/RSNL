# A simple regular expression tag filter

### CLASSDEF

setClass("RegexTagFilter",
  representation("FunctionalTagFilter", pattern="Pattern"))

### GENERICS

setGeneric("RegexTagFilter",
  function (pattern, negate=FALSE) standardGeneric("RegexTagFilter"))

### CONSTRUCTOR

setMethod("RegexTagFilter", "Pattern",
  function (pattern, negate=FALSE) {
    fun <- function (x) re.matchesPart(pattern, x)
    new("RegexTagFilter", FunctionalTagFilter(fun, negate, FALSE),
                            pattern=pattern)
  })

setMethod("RegexTagFilter", "character",
  function (pattern, negate=FALSE)
    RegexTagFilter(re.compile(pattern), negate))

### ACCESSORS

setMethod("pattern", "RegexTagFilter",
  function (object) object@pattern)

### SHOW

setMethod("show", "RegexTokenFilter",
  function (object)
    if (isNegated(object))
      cat("A negated regular expression tag filter using pattern:\n",
          re.pattern(pattern(object)), "\n")
    else
      cat("A regular expression tag filter using pattern:\n",
          re.pattern(pattern(object)), "\n"))
