# A simple functor object for encapsulating tag filter functions

### CLASSDEF

setClass("FunctionalTagFilter",
  representation("TokenFilter", "TagSafe", fun="function",
                 negate="logical", applyToEach="logical"))

### GENERICS

setGeneric("FunctionalTagFilter",
  function(fun, negate, applyToEach)
    standardGeneric("FunctionalTagFilter"))

### CONSTRUCTOR

setMethod("FunctionalTagFilter", signature(fun="function", 
                                             negate="logical",
                                             applyToEach="logical"),
  function (fun, negate, applyToEach)
    new("FunctionalTagFilter", fun=fun, negate=negate,
                                 applyToEach=applyToEach))

setMethod("FunctionalTagFilter", signature(fun="function",
                                             negate="missing",
                                             applyToEach="logical"),
  function (fun, negate, applyToEach)
    FunctionalTagFilter(fun, FALSE, applyToEach))

setMethod("FunctionalTagFilter", signature(fun="function",
                                             negate="missing",
                                             applyToEach="missing"),
  function (fun, negate, applyToEach)
    FunctionalTagFilter(fun, FALSE, FALSE))

setMethod("FunctionalTagFilter", signature(fun="function",
                                             negate="logical",
                                             applyToEach="missing"),
  function (fun, negate, applyToEach)
    FunctionalTagFilter(fun, negate, FALSE))

### ACCESSORS

setMethod("func", "FunctionalTagFilter",
  function (object) object@fun)

setMethod("isNegated", "FunctionalTagFilter",
  function (object) object@negate)

setMethod("isAppliedToEach", "FunctionalTagFilter",
  function (object) object@applyToEach)

### FILTER overriding

# Tag filters only work on Tagged objects, so no auto-tagging here
setMethod("filterTokens", signature(object="Tagged", 
                                    filter="FunctionalTagFilter",
                                    tokenizer="missing", index="missing"), 
  function(object, filter, tokenizer, index)  {
    fun <- func(filter)

    if (isNegated(filter))
      fn <- function (x) ! fun(x)
    else
      fn <- fun

    if (isAppliedToEach(filter))
      object[sapply(label(object), fn)]
    else
      object[ fn(label(object)) ]
  })
  
# This is necessary to avoid segfault from indeterminate method search
setMethod("filterTokens", signature(object="character", 
                                    filter="FunctionalTagFilter",
                                    tokenizer="missing", index="missing"),
  function (object, filter, tokenizer, index)
    stop("FunctonalTagFilters only operate on tagged token representations"))
 
### SHOW

setMethod("show", "FunctionalTagFilter",
  function (object)
    if (isNegated(object))
      cat("A negated functional tag filter using function:\n",
          format(func(object)), "\n")
    else
      cat("A functional tag filter using function:\n",
          format(func(object)), "\n"))
