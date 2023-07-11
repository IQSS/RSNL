# A simple token filter object that removes any all-punctuation
# tokens.

### CLASS DEF

setClass("PunctTokenFilter", representation("RegexTokenFilter"))

### CONSTRUCTION

PunctTokenFilter <- function () new("PunctTokenFilter",
  RegexTokenFilter('^\\p{Punct}$', TRUE))

### SHOW

setMethod("show", "PunctTokenFilter",
  function (object)
    cat("A token filter that removes all-punctuation tokens.\n"))
