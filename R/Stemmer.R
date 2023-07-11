# a base class for stemmers

### CLASS DEF

setClass("Stemmer", representation("TokenTransform"))

### GENERICS

setGeneric("stem",
  function(object, stemmer, tokenizer, index) standardGeneric("stem"))

### METHODS

# This works like it does for TokenFilters.  See TokenFilter.R for
# details.

# Redirect stem calls to transformTokens.
setMethod("stem", signature(stemmer="Stemmer"),
  function (object, stemmer, tokenizer, index)
    if (missing(tokenizer))
      if (missing(index))
        transformTokens(object, stemmer)
      else
        transformTokens(object, stemmer, index=index)
    else if (missing(index))
      transformTokens(object, stemmer, tokenizer)
    else
      transformTokens(object, stemmer, tokenizer, index))


setMethod("transformTokens", signature(object="character",
                                      transform="Stemmer",
                                      tokenizer="missing",
                                      index="missing"),
  function (object, transform, tokenizer, index)
    stem(object, transform))
