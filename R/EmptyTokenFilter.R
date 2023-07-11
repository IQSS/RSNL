# A simple token filter object for removing empty tokens.

### CLASS DEF

setClass("EmptyTokenFilter", representation("FunctionalTokenFilter"))

### CONSTRUCTION

EmptyTokenFilter <- function () {
  fun <- function (x) x != ""
  new("EmptyTokenFilter", FunctionalTokenFilter(fun, FALSE, FALSE))
}

### SHOW

setMethod("show", "EmptyTokenFilter",
  function (object)
    cat("A token filter that removes empty tokens.\n"))
