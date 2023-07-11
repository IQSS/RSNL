### CLASSDEF

# A virutal class that forms the base of classes used to view the
# content of a corpus element in a new way (these should probably be
# called CorpusElementViews because you can't generate one from a
# free-floating TextDocument object like you could from a RSNL
# Document in the old setup, but that name is just too clunky).  Note
# that these views are not full fledged views but simply helper
# objects for CorpusViews.  This is because TextDocuments are not
# ViewableObjects and the possibility of on-disk storage in Corpus
# objects makes a RSNLTextDocument type (RSNLPlainTextDocument, etc) that
# mirrors RSNLCorpus impractical.  
#
# The real solution here is to do database storage at the document
# level, not the corpus level, but that is another issue.

setClass("DocumentView",
    representation(
        "VIRTUAL",
        "View",
        index  = reftype("numeric")  # The index of this doc in the corpus
    )
)

### GENERICS
setGeneric("document", function (object) standardGeneric("document"))

setGeneric("index", function (object) standardGeneric("index"))

### ACCESSORS
setMethod("index", "DocumentView",
  function (object) ref(object, "index"))

# Returns the document of this view
setMethod("document", "DocumentView",
  function(object) return(object@corpus[[index(object)]]))
