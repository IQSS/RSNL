# A wrapper fro Java's Pattern class
setClass("Pattern", representation(ref="jobjRef"))

### GENERICS

setGeneric("re.flags", function (pattern) standardGeneric("re.flags"))

setGeneric("re.matcher",
  function (pattern, input) standardGeneric("re.matcher"))

setGeneric("re.pattern", function (object) standardGeneric("re.pattern"))

setGeneric("re.split",
  function (pattern, input, limit=0) standardGeneric("re.split"))

setGeneric("re.findall",
  function (pattern, input) standardGeneric("re.findall"))

# Two custom methods
setGeneric("re.matchesPart", 
  function (object, input) standardGeneric("re.matchesPart"))

setGeneric("re.sub",
  function (object, input, repl) standardGeneric("re.sub"))

### HELPER
str2charseq <- function (string)
  .jcast(.jnew("java/lang/String", string), "java/lang/CharSequence")

### CONSTRUCTOR

re.compile <- function (pattern, unix.lines=FALSE, case.insensitive=FALSE,
                        comments=FALSE, multiline=FALSE, literal=FALSE,
                        dotall=FALSE, unicode.case=TRUE, canon.eq=TRUE)
{
  flagint <- 0
  if (unix.lines) flagint <- flagint + 1
  if (case.insensitive) flagint <- flagint + 2
  if (comments) flagint <- flagint + 4
  if (multiline) flagint <- flagint + 8
  if (literal) flagint <- flagint + 16
  if (dotall) flagint <- flagint + 32
  if (unicode.case) flagint <- flagint + 64
  if (canon.eq) flagint <- flagint + 128

  obj <- NULL
  if (flagint > 0)
    obj <- .jcall("java/util/regex/Pattern", "Ljava/util/regex/Pattern;", 
                  "compile", pattern, as.integer(flagint))
  else
    obj <- .jcall("java/util/regex/Pattern", "Ljava/util/regex/Pattern;", 
                  "compile", pattern)

  new("Pattern", ref=obj)
}

### METHODS

setMethod("re.flags", signature(pattern="Pattern"),
  function (pattern) {
    flagstr <- c("UNIX_LINES", "CASE_INSENSITIVE", "COMMENTS",
                 "MULTILINE", "LITERAL", "DOTALL", "UNICODE_CASE",
                 "CANON_EQ");
    flags <- character(0)
    flagint <- .jcall(pattern@ref, "I", "flags")

    for (i in 7:0) {
      if (flagint >= 2^i) {
        flagint <- flagint - 2^i
        flags <- c(flagstr[i+1], flags)
      }
    }
    flags
  })

setMethod("re.matcher", signature(pattern="Pattern", input="character"),
  function (pattern, input) {
    if (length(input) != 1)
      stop("re.matcher: input must be scalar")
    obj <- .jcall(pattern@ref, "Ljava/util/regex/Matcher;", "matcher", 
                  str2charseq(input))
    new("Matcher", ref=obj)
  })

setMethod("re.pattern", signature(object="Pattern"),
  function (object) .jcall(object@ref, "S", "pattern"))

setMethod("re.split", signature(pattern="Pattern", input="character"),
  function (pattern, input, limit=0) {
    if (length(input) != 1)
      stop("re.split: input must be scalar")
    .jcall(pattern@ref, "[S", "split", str2charseq(input), as.integer(limit))
  })

setMethod("re.findall", signature(pattern="Pattern", input="character"),
  function (pattern, input) {
    if (length(input) != 1)
      stop("re.findall: input must be scalar")
    .jcall("RegexBridge", "[S", "findall", pattern@ref, input)
  })

# A couple methods for applying matcher methods to vectors of inputs

# Returns vector of T/F for matches
setMethod("re.matchesPart", signature(object="Pattern", input="character"),
  function (object, input) {
    if (length(input) == 0)
      stop("re.matchesPart: input must contain at least one element")
    .jcall("RegexBridge", "[Z", "matchesPart", object@ref, input)
  })

# Like replaceAll, but works for vectors of input
setMethod("re.sub", signature(object="Pattern", input="character",
                              repl="character"),
  function (object, input, repl) {
    if (length(input) == 0)
      stop("re.stop: input must contain at least one element")
    if (length(repl) == 0)
      stop("re.stop: repl must contain at least one element")
    if (length(repl) > 1 & length(input) != length(repl))
      repl <- rep(repl, length.out=length(input))
    .jcall("RegexBridge", "[S", "sub", object@ref, input, repl)
  })
      

### SHOW
setMethod("show", "Pattern",
  function (object) cat("Pattern: ", re.pattern(object), "\n"))
