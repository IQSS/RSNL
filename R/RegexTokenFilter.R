# A simple regular expression token filter

### CLASSDEF

setClass("RegexTokenFilter",
  representation("FunctionalTokenFilter", pattern="Pattern"))

### GENERICS

setGeneric("RegexTokenFilter",
  function (pattern, negate=FALSE) standardGeneric("RegexTokenFilter"))

### CONSTRUCTOR

setMethod("RegexTokenFilter", "Pattern",
  function (pattern, negate=FALSE) {
    fun <- function (x) re.matchesPart(pattern, x)
    new("RegexTokenFilter", FunctionalTokenFilter(fun, negate, FALSE),
                            pattern=pattern)
  })

setMethod("RegexTokenFilter", "character",
  function (pattern, negate=FALSE)
    RegexTokenFilter(re.compile(pattern), negate))

### ACCESSORS

setMethod("pattern", "RegexTokenFilter",
  function (object) object@pattern)

### SHOW

setMethod("show", "RegexTokenFilter",
  function (object)
    if (isNegated(object))
      cat("A negated regular expression token filter using pattern:\n",
          re.pattern(pattern(object)), "\n")
    else
      cat("A regular expression token filter using pattern:\n",
          re.pattern(pattern(object)), "\n"))
