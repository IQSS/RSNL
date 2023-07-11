# A simple token transform object for lower-casing tokens.

### CLASS DEF

setClass("TolowerTokenTransform", 
  representation("TagSafeFunctionalTokenTransform"))

### CONSTRUCTION

TolowerTokenTransform <- function () {
  fun <- function(x)
    if(is(x, "Tagged"))
      Tagged(tolower(x), label(x))
    else
      tolower(x)
  new("TolowerTokenTransform", FunctionalTokenTransform(fun, TRUE, FALSE))
}

### SHOW

setMethod("show", "TolowerTokenTransform",
  function (object)
    cat("A token transform that converts all tokens to lowercase.\n"))
