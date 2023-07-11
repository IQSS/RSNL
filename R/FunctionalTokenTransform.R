#  A simple functor-style object for encapsulating transform functions

### CLASSDEF

setClass("FunctionalTokenTransform",
  representation("TokenTransform", fun="function", applyToEach="logical"))

setClass("TagSafeFunctionalTokenTransform",
  representation("FunctionalTokenTransform", "TagSafe"))

### GENERICS

setGeneric("FunctionalTokenTransform",
  function (fun, tagSafe, applyToEach)
    standardGeneric("FunctionalTokenTransform"))

### CONSTRUCTION

setMethod("FunctionalTokenTransform",
          signature(fun="function", tagSafe = "logical",
                    applyToEach="logical"),
  function (fun, tagSafe, applyToEach)
    if (tagSafe)
      new("TagSafeFunctionalTokenTransform", fun=fun, 
          applyToEach=applyToEach)
    else
      new("FunctionalTokenTransform", fun=fun, applyToEach=applyToEach))

setMethod("FunctionalTokenTransform",
          signature(fun="function", tagSafe = "logical",
                    applyToEach="missing"),
  function (fun, tagSafe, applyToEach)
    FunctionalTokenTransform(fun, tagSafe, FALSE))

setMethod("FunctionalTokenTransform",
          signature(fun="function", tagSafe = "missing",
                    applyToEach = "logical"),
  function (fun, tagSafe, applyToEach)
    FunctionalTokenTransform(fun, FALSE, tagSafe))


setMethod("FunctionalTokenTransform",
          signature(fun="function", tagSafe = "missing",
                    applyToEach = "missing"),
  function (fun, tagSafe, applyToEach)
    FunctionalTokenTransform(fun, FALSE, FALSE))

### ACCESSORS

setMethod("func", "FunctionalTokenTransform",
  function (object) object@fun)

setMethod("isAppliedToEach", "FunctionalTokenTransform",
  function (object) object@applyToEach)

### TRANSFORM overriding

setMethod("transformTokens", signature(object="character",
                                       transform="FunctionalTokenTransform",
                                       tokenizer="missing",
                                       index="missing"),
  function (object, transform, tokenizer, index) {
    if (is(object, "TextDocument"))
      object <- tokenize(object)

     fun <- func(transform)

     if (isAppliedToEach(transform))
      sapply(object, fun, USE.NAMES=FALSE)
     else
      fun(object)
   })

### SHOW

setMethod("show", "FunctionalTokenTransform",
  function (object)
    if (isAppliedToEach(object))
      cat("A functional token transform applying to each token the function:\n",
          format(func(object)), "\n")
    else
      cat("A functional token transform using function:\n",
          format(func(object)), "\n"))
