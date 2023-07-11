# An extension "character" with a (string) tag for each string.
# Objects of this type behave just like characters except they print
# to the screen in "token/tag" format and they provide
# accessors/modifiers to the tags.  Also, c() is modified to keep tag
# info and match matches on tags and tokens (so you want to do
# match(as.character(x), y) if is(x, "Tagged") and you just want to
# match the raw character vector.

### CLASSDEF

setClass("Tagged", representation("character", tag="character"))

### GENERICS

setGeneric("label", function(object) standardGeneric("label"))
setGeneric("label<-", function (object, value) standardGeneric("label<-"))
setGeneric("flatten", function(object) standardGeneric("flatten"))

### CONSTRUCTION

Tagged <- function (value=character(), tag=character())
{
  if (!is.character(value))
    stop(paste("Bad token: ", value))
  if (!is.character(tag))
    stop(paste("Bad tag: ", tag))
  tag = rep(tag, length.out=length(value))
  
  new("Tagged", .Data=value, tag=tag)
}

### ACCESSORS

setMethod("label", signature("Tagged"),
  function (object) object@tag)

setMethod("[", signature(x="Tagged", j="missing", drop="missing"),
  function (x, i, j, drop) Tagged(x@.Data[i], x@tag[i]))

### MODIFIERS

setMethod("label<-", "Tagged",
  function (object, value) {
    if (!is.character(value))
      stop(paste("Bad tag: ", value))
    object@tag <- rep(value, length.out=length(object))
    object
  })  


### ETC

# Returns a pure character object in "token/tag" form
setMethod("flatten", signature(object="Tagged"),
  function (object) paste(object@.Data, object@tag, sep="/"))

# XXX %in% doesn't pick this up!!!
#setMethod("match", signature(x="Tagged", table="Tagged"),
#  function (x, table, nomatch = NA_integer_, incomparables = NULL)
#    match(flatten(x), flatten(table), nomatch, incomparables))

setMethod("c", signature(x = "Tagged"),
  function(x, ..., recursive=FALSE) {
    args <- list(...)
    tokens <- x@.Data
    tags <- label(x)
    for (arg in args) {
      tag <- rep(NA, length(arg))
      if (is(arg, "Tagged"))
        tag <- label(arg)
      tokens <- c(tokens, arg)
      tags <- c(tags, tag)
    }
    Tagged(tokens, tags)
  })

### SHOW

setMethod("show", "Tagged",
  function (object) print(flatten(object)))
