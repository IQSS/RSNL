# A simple functor object for encapsulating filter functions

### CLASSDEF

setClass("FunctionalTokenFilter",
  representation("TokenFilter", "TagSafe", fun="function",
                 negate="logical", applyToEach="logical"))

### GENERICS

setGeneric("FunctionalTokenFilter",
  function(fun, negate, applyToEach)
    standardGeneric("FunctionalTokenFilter"))

### CONSTRUCTOR

setMethod("FunctionalTokenFilter", signature(fun="function", 
                                             negate="logical",
                                             applyToEach="logical"),
  function (fun, negate, applyToEach)
    new("FunctionalTokenFilter", fun=fun, negate=negate,
                                 applyToEach=applyToEach))

setMethod("FunctionalTokenFilter", signature(fun="function",
                                             negate="missing",
                                             applyToEach="logical"),
  function (fun, negate, applyToEach)
    FunctionalTokenFilter(fun, FALSE, applyToEach))

setMethod("FunctionalTokenFilter", signature(fun="function",
                                             negate="missing",
                                             applyToEach="missing"),
  function (fun, negate, applyToEach)
    FunctionalTokenFilter(fun, FALSE, FALSE))

setMethod("FunctionalTokenFilter", signature(fun="function",
                                             negate="logical",
                                             applyToEach="missing"),
  function (fun, negate, applyToEach)
    FunctionalTokenFilter(fun, negate, FALSE))

### ACCESSORS

setMethod("func", "FunctionalTokenFilter",
  function (object) object@fun)

setMethod("isNegated", "FunctionalTokenFilter",
  function (object) object@negate)

setMethod("isAppliedToEach", "FunctionalTokenFilter",
  function (object) object@applyToEach)

### FILTER overriding
# Filters the given string, or vector of tokens, but always break a
# TextDocument down into its tokens first.
setMethod("filterTokens", signature(object="character", 
                                    filter="FunctionalTokenFilter",
                                    tokenizer="missing", index="missing"), 
  function(object, filter, tokenizer, index)  {
    if (is(object, "TextDocument"))
      object <- tokenize(object)

    fun <- func(filter)

    if (isNegated(filter))
      fn <- function (x) ! fun(x)
    else
      fn <- fun

    if (isAppliedToEach(filter))
      object[sapply(object, fn)]
    else
      object[ fn(object) ]
  })
 
### SHOW

setMethod("show", "FunctionalTokenFilter",
  function (object)
    if (isNegated(object))
      cat("A negated functional token filter using function:\n",
          format(func(object)), "\n")
    else
      cat("A functional token filter using function:\n",
          format(func(object)), "\n"))
