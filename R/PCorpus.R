# Proof of concept persistent (both on disk and in memory) tm Corpus
# object.  A better setup would use a direct implementation of Corpus
# based on reference slots within tm itself, but this approach allows
# us to prototype RSNL with a vanilla tm installation.

# This setup has unused slots (it redirects all accesses to the
# underlying reference wrapper object) and will not perfectly
# impersonate a Corpus object with respect to list operations (for
# example, while seq works seq_along behaves inappropriately).

# Bug in RClass.R with extending basic object types with RObjects
# needs to be fixed.  For now, use two levels of redirection.
setClass("cref", representation(ref=reftype("Corpus")))

setClass("RSNLCorpus",
  representation("ViewableObject", "Corpus", ref="cref"))

### GENERICS

setGeneric("RSNLCorpus", function (corpus) standardGeneric("RSNLCorpus"))

### CONSTRUCTION

setMethod("RSNLCorpus", "Corpus",
  function (corpus) {
    ref <- new("cref", ref=corpus)
    new("RSNLCorpus", ViewableObject(), ref=ref)
  })

setMethod("clone", "RSNLCorpus",
  function (x) {
    ref <- clone(x@ref)
    new("RSNLCorpus", ViewableObject(), ref=ref)
  })

### Redirect slot accessor methods

setMethod("DMetaData", "RSNLCorpus",
  function (object) DMetaData(ref(object@ref, "ref")))

setReplaceMethod("DMetaData", "RSNLCorpus",
  function (x, value) {
    DMetaData(ref(x@ref, "ref")) <- value
    updateViews(x)
    x
  })

setMethod("CMetaData", "RSNLCorpus",
  function(object) CMetaData(ref(object@ref, "ref")))

setMethod("DBControl", "RSNLCorpus",
  function(object) DBControl(ref(object@ref, "ref")))

### Override methods that access slots directly and also methods that
### "modify" objects

setReplaceMethod("meta", signature(object = "RSNLCorpus"),
  function(object, tag, type = "indexed", value) {
    meta(ref(object@ref, "ref"), tag, type) <- value
    updateViews(x)
    object
  })

# XXX Not sure if promotion and demotion is the way to go here...
setMethod("[", signature(x = "RSNLCorpus", i = "ANY", j = "ANY", drop = "ANY"),
  function(x, i, j, ... , drop)
    RSNLCorpus(ref(x@ref,"ref")[i, ..., drop]))

setMethod("[<-", 
          signature(x = "RSNLCorpus", i = "ANY", j = "ANY", value = "ANY"),
  function(x, i, j, ... , value) {
    if (is(value, "RSNLCorpus"))  # demote
      value <- ref(value@ref, "ref")
    ref(x@ref, "ref")[i, ..., drop] <- value
    updateViews(x, index=i)
    x
  })

setMethod("[[",
          signature(x = "RSNLCorpus", i = "ANY", j = "ANY"),
   function(x, i, j, ...) ref(x@ref,"ref")[[i, ...]])

setMethod("[[<-",
          signature(x = "RSNLCorpus", i = "ANY", j = "ANY", value = "ANY"),
  function(x, i, j, ..., value) {
    ref(x@ref, "ref")[[i, ...]] <- value
    updateViews(x, index=i)
    x
  })

# XXX broken c2 issue!
#setMethod("c", signature(x = "RSNLCorpus"),
#          function(x, ..., meta = list(merge_date = Sys.time(), 
#                                       merger = Sys.getenv("LOGNAME")),
#                   recursive = TRUE)
#    c(ref(x@ref, "ref"), ..., meta, recursive))

#setGeneric("c2", function(x, y, ..., meta = list(merge_date = Sys.time(), merger = Sys.getenv("LOGNAME")), recursive = TRUE) standardGeneric("c2"))
#setMethod("c2", signature(x = "Corpus", y = "Corpus"),
#  function(x, y, ..., meta = list(merge_date = Sys.time(), 
#                                  merger = Sys.getenv("LOGNAME")),
#           recursive = TRUE)
#    tm:::c2(ref(x@ref, "ref"), ref(y@ref, "ref"), ..., meta, merger))
 
### XXX DBP - these three methods no longer seem necessary with the
### current corpus setup in tm

# Note that this appends w/out an assignment...
#setMethod("appendMeta",
#          signature(object = "RSNLCorpus"),
#  function(object, cmeta = NULL, dmeta = NULL) {
#    ref(object@ref, "ref") <- 
#      appendMeta(ref(object@ref, "ref"), cmeta, dmeta)
#    updateViews(object)
#    object
#  })

#setMethod("removeMeta",
#          signature(object = "RSNLCorpus"),
#  function(object, cname = NULL, dname = NULL) {
#    ref(object@ref, "ref") <-
#      removeMeta(ref(object@ref, "ref"), cmeta, dmeta)
#    updateViews(object)
#    object
#  })

# XXX Not sure we want auto-updating semantics here or not
#setMethod("prescindMeta",
#          signature(object = "RSNLCorpus", meta = "character"),
#  function(object, meta) {
#    ref(object@ref, "ref") <-
#      prescindMeta(ref(object@ref, "ref"), meta)
#    updateViews(object)
#    object
#  })
 
setMethod("appendElem",
          signature(object = "RSNLCorpus", data = "TextDocument"),
  function(object, data, meta = NULL) {
    ref(object@ref, "ref") <-
      appendElem(ref(object@ref, "ref"), data, meta)
    updateViews(object, newelem=TRUE)
    object
  })

setMethod("tmMap",
          signature(object = "RSNLCorpus", FUN = "function"),
  function(object, FUN, ..., lazy = FALSE) {
    ref(object@ref, "ref") <-
      tmMap(ref(object@ref, "ref"), FUN=FUN, ..., lazy=lazy)
    updateViews(object, index=1:length(object))
    object
  })

# Now cover everything that wasn't already covered that uses lapply or
# sapply.  Also redefine lapply and sapply for basic list operations.
# This is probably not nearly all we need to do to make RSNLCorpus
# objects act like actual lists...

setMethod("lapply", signature(X = "RSNLCorpus"),
  function (X, FUN, ...) {
    lapply(ref(X@ref, "ref"), FUN, ...)})

if (! isGeneric("sapply")) {
  if (is.function("sapply"))
    fun <- sapply
  else
    fun <- function(X, FUN, ..., simplify=TRUE, USE.NAMES=TRUE)
  setGeneric("sapply", fun)
}
setMethod("sapply", signature(X = "RSNLCorpus"),
  function (X, FUN, ..., simplify=TRUE, USE.NAMES=TRUE) {
    sapply(ref(X@ref, "ref"), FUN, ..., simplify=simplify, 
           USE.NAMES=USE.NAMES)})

# XXX Also seems unnecessary now.
#setMethod("tmUpdate", signature(object = "RSNLCorpus", origin = "DirSource"),
#  function(object, origin,
#           readerControl = list(reader = origin@DefaultReader,
#                                language = "en_US", load = TRUE),
#           ...) {
#    res <- RSNLCorpus(tmUpdate(ref(object@ref, "ref"), origin,
#                                                    readerControl, ...))
#    updateViews(object)
#    res
#  })

setMethod("inspect", "RSNLCorpus",
  function (x) inspect(ref(x@ref, "ref")))

setMethod("meta", "RSNLCorpus",
  function (object, tag = NULL, type = "indexed")
    meta(ref(object@ref, "ref"), tag, type))

setMethod("tmFilter", signature(object = "RSNLCorpus"),
  function(object, ..., FUN = sFilter, doclevel = FALSE) 
    RSNLCorpus(tmFilter(ref(object@ref, "ref"), ..., FUN=FUN, doclevel=doclevel)))

setMethod("tmIndex", signature(object = "RSNLCorpus"),
  function(object, ..., FUN = sFilter, doclevel = FALSE) 
    tmIndex(ref(object@ref, "ref"), ..., FUN=FUN, doclevel=doclevel))



### Quick hack to materialize

# XXX updateViews??
materialize <- function(corpus, range = seq_along(corpus)) {
    if (is(corpus, "RSNLCorpus"))
      corpus <- ref(corpus@ref, "ref")
    lazyTmMap <- meta(corpus, tag = "lazyTmMap", type = "corpus")
    if (!is.null(lazyTmMap)) {
       # Make valid and lazy index
       idx <- (seq_along(corpus) %in% range) & lazyTmMap$index
       if (any(idx)) {
           res <- lapply(corpus@.Data[idx], loadDoc)
           for (m in lazyTmMap$maps)
               res <- lapply(res, m, DMetaData = DMetaData(corpus))
           corpus@.Data[idx] <- res
           lazyTmMap$index[idx] <- FALSE
       }
    }
    # Clean up if everything is materialized
    if (!any(lazyTmMap$index))
        lazyTmMap <- NULL
    meta(corpus, tag = "lazyTmMap", type = "corpus") <- lazyTmMap
    return(corpus)
}

### Other things that need overriding
setMethod("length", "RSNLCorpus",
  function (x) length(ref(x@ref, "ref")))
