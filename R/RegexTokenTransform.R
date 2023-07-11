# A simple regular expression token transform

### CLASSDEF

setClass("RegexTokenTransform",
  representation("TagSafeFunctionalTokenTransform", pattern="Pattern",
                 repl="character"))

### GENERICS

setGeneric("RegexTokenTransform",
  function (pattern, repl="")
    standardGeneric("RegexTokenTransform"))

setGeneric("replacementString",
  function (object) standardGeneric("replacementString"))

### CONSTRUCTOR

setMethod("RegexTokenTransform", signature("Pattern", "character"),
  function (pattern, repl="") {
    if (length(repl) != 1)
      stop("Replacement string vector should be of length 1.")
    fun <- function (x)
      if (is(x, "Tagged"))
        Tagged(re.sub(pattern, x, repl), label(x))
      else
        re.sub(pattern, x, repl)
    new("RegexTokenTransform", FunctionalTokenTransform(fun, TRUE, FALSE),
                               pattern=pattern, repl=repl)
  })

setMethod("RegexTokenTransform", signature("character", "character"),
  function (pattern, repl="")
    RegexTokenTransform(re.compile(pattern), repl))

# XXX No default for now

### ACCESSORS

setMethod("pattern", "RegexTokenTransform",
  function (object) object@pattern)

setMethod("replacementString", "RegexTokenTransform",
  function (object) object@repl)

### SHOW

setMethod("show", "RegexTokenTransform",
  function (object)
    cat("A regular expression token transform with replacement string ",
        replacementString(object), ", and using pattern:\n",
        re.pattern(pattern(object)), "\n"))
