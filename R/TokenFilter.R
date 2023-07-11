# TokenFilter - a base class for objects that filter out certain
# tokens represented by a TokenizedView

### CLASS DEF

setClass("TokenFilter", representation("TokenTransform"))

### GENERICS

setGeneric("filterTokens",
  function(object, filter, tokenizer, index)
    standardGeneric("filterTokens"))

### METHODS

# The transformTokens method provides all of the underlying functionality
# here.  We just make filterTokens an alias for it.  This works by
# redirecting most calls to filterTokens to transformTokens and by
# redirecting transformTokens back to filterTokens for the base case
# which extending classes of TokenFilter should implement.

setMethod("filterTokens", signature(filter="TokenFilter"),
  function (object, filter, tokenizer, index)
    if (missing(tokenizer))
      if (missing(index))
        transformTokens(object, filter)
      else
        transformTokens(object, filter, index=index)
    else if (missing(index))
        transformTokens(object, filter, tokenizer)
    else
      transformTokens(object, filter, tokenizer, index))

setMethod("transformTokens", signature(object="character",
                                    transform="TokenFilter",
                                    tokenizer="missing",
                                    index="missing"),
  function (object, transform, tokenizer, index)
    filterTokens(object, transform))
