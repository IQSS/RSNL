# A wrapper for Java's Matcher class
setClass("Matcher", representation(ref="jobjRef"))

### GENERICS

setGeneric("re.end", function (object, group) standardGeneric("re.end"))

setGeneric("re.find",
  function (object, start) standardGeneric("re.find"))

setGeneric("re.group",
  function (object, group) standardGeneric("re.group"))

setGeneric("re.groupCount",
  function (object) standardGeneric("re.groupCount"))

setGeneric("re.hasAnchoringBounds",
  function (object) standardGeneric("re.hasAnchoringBounds"))

setGeneric("re.hasTransparentBounds",
  function (object) standardGeneric("re.hasTransparentBounds"))

setGeneric("re.hitEnd",
  function (object) standardGeneric("re.hitEnd"))

setGeneric("re.lookingAt",
  function (object) standardGeneric("re.lookingAt"))

setGeneric("re.matches",
  function (object) standardGeneric("re.matches"))

setGeneric("re.region",
  function(object, start, end) standardGeneric("re.region"))

setGeneric("re.regionEnd",
  function (object) standardGeneric("re.regionEnd"))

setGeneric("re.regionStart",
  function (object) standardGeneric("re.regionStart"))

setGeneric("re.replaceAll",
  function (object, replacement) standardGeneric("re.replaceAll"))

setGeneric("re.replaceFirst",
  function (object, replacement) standardGeneric("re.replaceFirst"))

setGeneric("re.requireEnd",
  function (object) standardGeneric("re.requireEnd"))

setGeneric("re.reset",
  function (object, input) standardGeneric("re.reset"))

setGeneric("re.start",
  function (object, group=0) standardGeneric("re.start"))

setGeneric("re.useAnchoringBounds",
  function (object, b) standardGeneric("re.useAnchoringBounds"))

setGeneric("re.usePattern",
  function (object, pattern) standardGeneric("re.usePattern"))

setGeneric("re.useTransparentBounds",
  function (object, b) standardGeneric("re.useTransparentBounds"))

### METHODS

setMethod("re.end", signature(object="Matcher", group="numeric"),
  function (object, group) {
    if (length(group) != 1)
      stop("re.end: group must be scalar")
    .jcall(object@ref, "I", "end", as.integer(group))
  })

setMethod("re.end", signature(object="Matcher", group="missing"),
  function (object, group) .jcall(object@ref, "I", "end"))

setMethod("re.find", signature(object="Matcher", start="numeric"),
  function (object, start) {
    if (length(start) != 1)
      stop("re.find: start must be scalar")
    .jcall(object@ref, "Z", "find", as.integer(start))
  })
  
setMethod("re.find", signature(object="Matcher", start="missing"),
  function (object, start) .jcall(object@ref, "Z", "find"))

setMethod("re.group", signature(object="Matcher", group="numeric"),
  function (object, group) {
    if (length(group) != 1)
      stop("re.group: group must be scalar")
    .jcall(object@ref, "S", "group", as.integer(group))
  })

setMethod("re.group", signature(object="Matcher", group="missing"),
  function (object, group) .jcall(object@ref, "S", "group"))

setMethod("re.groupCount", signature(object="Matcher"),
  function (object) .jcall(object@ref, "I", "groupCount"))

setMethod("re.hasAnchoringBounds", signature(object="Matcher"),
  function (object) .jcall(object@ref, "Z", "hasAnchoringBounds"))

setMethod("re.hasTransparentBounds", signature(object="Matcher"),
  function (object) .jcall(object@ref, "Z", "hasTransparentBounds"))

setMethod("re.hitEnd", signature(object="Matcher"),
  function (object) .jcall(object@ref, "Z", "hitEnd"))

setMethod("re.lookingAt", signature(object="Matcher"),
  function (object) .jcall(object@ref, "Z", "lookingAt"))

setMethod("re.matches", signature(object="Matcher"),
  function (object) .jcall(object@ref, "Z", "matches"))

setMethod("re.pattern", signature(object="Matcher"),
  function (object) new("Pattern",
    ref=.jcall(object@ref, "Ljava/util/regex/Pattern;", "pattern")))

setMethod("re.region", signature(object="Matcher", start="numeric",
                                 end="numeric"),
  function (object, start, end) {
    if (length(start) != 1 | length(end) != 1)
      stop("re.region: start and end must be scalars")
    .jcall(object@ref, "Ljava/util/regex/Matcher;", "region", 
      as.integer(start), as.integer(end))
    object
  })

setMethod("re.regionEnd", signature(object="Matcher"),
  function (object) .jcall(object@ref, "I", "regionEnd"))

setMethod("re.regionStart", signature(object="Matcher"),
  function (object) .jcall(object@ref, "I", "regionStart"))

setMethod("re.replaceAll", signature(object="Matcher",
                                     replacement="character"),
  function (object, replacement) {
    if (length(replacement) != 1)
      stop("re.replaceAll: replacement must be scalar")
    .jcall(object@ref, "S", "replaceAll", replacement)
  })

setMethod("re.replaceFirst", signature(object="Matcher",
                                       replacement="character"),
  function (object, replacement) {
    if (length(replacement) != 1)
      stop("re.replaceFirst: replacement must be scalar")
    .jcall(object@ref, "S", "replaceFirst", replacement)
  })

setMethod("re.requireEnd", signature(object="Matcher"),
  function (object) .jcall(object@ref, "Z", "requireEnd"))

setMethod("re.reset", signature(object="Matcher", input="character"),
  function (object, input) {
    if (length(input) != 1)
      stop("re.reset: input must be scalars")
    obj <- .jcall(object@ref, "Ljava/util/regex/Matcher;", "reset", 
                  str2charseq(input))
    object@ref<-obj
    object
  })

setMethod("re.reset", signature(object="Matcher", input="missing"),
  function (object, input) {
    obj <- .jcall(object@ref, "Ljava/util/regex/Matcher;", "reset")
    object@ref<-obj
    object
  })

setMethod("re.start", signature(object="Matcher", group="numeric"),
  function (object, group) .jcall(object@ref, "I", "start", group))

setMethod("re.start", signature(object="Matcher", group="missing"),
  function (object, group) .jcall(object@ref, "I", "start"))

setMethod("re.useAnchoringBounds", signature(object="Matcher", b="logical"),
  function (object, b) {
    if (length(b) != 1)
      stop("re.useAnchoringBounds: b must be scalar")
    .jcall(object@ref, "Ljava/util/regex/Matcher;", "useAnchoringBounds", b)
    object
  })

setMethod("re.usePattern", signature(object="Matcher", pattern="Pattern"),
  function (object, pattern) {
    .jcall(object@ref, "Ljava/util/regex/Matcher;", "usePattern", pattern)
    object
  })

setMethod("re.useTransparentBounds", 
    signature(object="Matcher", b="logical"),
  function (object, b) {
    if (length(b) != 1)
      stop("re.useTransparentBounds: b must be scalar")
    .jcall(object@ref, "Ljava/util/regex/Matcher;",
           "useTransparentBounds", b)
    object
  })

setMethod("show", "Matcher",
  function (object) cat("A Matcher using Pattern: ", 
                        re.pattern(re.pattern(object)), "\n"))
