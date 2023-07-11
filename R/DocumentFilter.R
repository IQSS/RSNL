# DocumentFilter - a base class for filters that screen certain
# documents out of corpus views.

### CLASS DEF

# In CorpusView.R
# setClass("DocumentFilter", representation("VIRTUAL"))

### GENERICS

setGeneric("filterDocuments",
  function(object, filter)
    standardGeneric("filterDocuments"))

### METHODS

# The method signature that all extenders must implement
# This list should be a list of DocumentView objects.
setMethod("filterDocuments", signature(object="list",
                                       filter="DocumentFilter"),
  function (object, filter)
    stop("filterDocuments must be implemented by derived class."))

# Override for TokenizedCVs
setMethod("filterDocuments", signature(object="TokenizedCorpusView",
                                       filter="DocumentFilter"),
  function (object, filter)
    TokenizedCorpusView(corpus(object), tokenizer(object),
                        tokenTransforms(object),
                        c(documentFilters(object), filter)))

# TaggedCVs
setMethod("filterDocuments", signature(object="TaggedCorpusView",
                                       filter="DocumentFilter"),
  function (object, filter)
    tag(filterDocuments(as(object, "TokenizedCorpusView"), filter),
        tagger(object)))
                                       
# Invoking filterDocument directly on a RSNLCorpus generates a filtered
# TokenizedCorpusView, tokenized with the default tokenizer.
setMethod("filterDocuments", signature(object="RSNLCorpus",
                                      filter="DocumentFilter"),
  function (object, filter)
    filterDocuments(tokenize(object), filter))
    
