# TokenTransform - a base class for objects that transform the tokens
# vector of a TokenizedView in any way.

### CLASS DEF

# In TokenizedView so TokenizedCorpusView and the like can know about
# it.  Looks like:
# setClass("TokenTransform", representation("VIRTUAL"))

### GENERICS

setGeneric("transformTokens",
  function(object, transform, tokenizer, index) 
    standardGeneric("transformTokens"))

### METHODS

# The method signature that all extenders must implement
setMethod("transformTokens", signature(object="character",
                                       transform="TokenTransform",
                                       tokenizer="missing",
                                       index="missing"),
  function (object, transform, tokenizer, index)
    stop("transformTokens must be implemented by derived class."))

# Calls the above method when a tokenizer is provided, after
# tokenizing
setMethod("transformTokens", signature(object="character",
                                       transform="TokenTransform",
                                       tokenizer="Tokenizer",
                                       index="missing"),
  function (object, transform, tokenizer, index)
    transformTokens(tokenize(object, tokenizer), transform))

# We've covered character objects, but one can also transform
# RSNLCorpus, TokenizedDocument, and TokenizedCorpus objects.  In the
# latter two cases the view provides tokenizer information and it
# makes no sense to pass a tokenizer along.

# For TokenizedCorpusView
setMethod("transformTokens", signature(object="TokenizedCorpusView",
                              transform="TokenTransform",
                              tokenizer="missing",
                              index="missing"),
  function (object, transform, tokenizer, index)
    TokenizedCorpusView(corpus(object), RSNL:::tokenizer(object),
                        c(tokenTransforms(object), transform), 
                        documentFilters(object)))

# For TokenizedCorpusView + index
setMethod("transformTokens", signature(object="TokenizedCorpusView",
                              transform="TokenTransform",
                              tokenizer="missing",
                              index="numeric"),
  function (object, transform, tokenizer, index)
    TokenizedDocumentView(corpus(object), RSNL:::tokenizer(object),
                          index, c(tokenTransforms(object), transform)))

# For RSNLCorpus
setMethod("transformTokens", signature(object="RSNLCorpus", 
                              transform="TokenTransform",
                              index="missing"),
  function(object, transform, tokenizer, index) 
    if (missing(tokenizer))
      transformTokens(tokenize(object), transform)
    else
      TokenizedCorpusView(object, tokenizer, list(transform)))

# For RSNLCorpus + index.
setMethod("transformTokens", signature(object="RSNLCorpus",
                              transform="TokenTransform",
                              index="numeric"),
  function (object, transform, tokenizer, index)
    if (missing(tokenizer))
      transformTokens(tokenize(object), transform, index=index)
    else
      TokenizedDocumentView(object, tokenizer, index, list(transform)))

# For TokenizedDocumentView.
setMethod("transformTokens", signature(object="TokenizedDocumentView",
                              transform="TokenTransform",
                              tokenizer="missing",
                              index="missing"),
  function (object, transform, tokenizer, index)
    TokenizedDocumentView(corpus(object), RSNL:::tokenizer(object),
                          RSNL:::index(object),
                          c(tokenTransforms(object), transform)))

# For Tagged views: do the tokview thing and apply the tagger
setMethod("transformTokens", signature(object="TaggedCorpusView",
                                    transform="TokenTransform",
                                    tokenizer="missing",
                                    index="missing"),
  function (object, transform, tokenizer, index)
    tag(transformTokens(as(object, "TokenizedCorpusView"), transform),
        tagger(object)))
      
setMethod("transformTokens", signature(object="TaggedCorpusView",
                              transform="TokenTransform",
                              tokenizer="missing",
                              index="numeric"),
  function (object, transform, tokenizer, index)
    tag(transformTokens(as(object, "TokenizedCorpusView"), transform,
                        index=index), tagger(object)))

setMethod("transformTokens", signature(object="TaggedDocumentView",
                                    transform="TokenTransform",
                                    tokenizer="missing",
                                    index="missing"),
  function (object, transform, tokenizer, index)
    tag(transformTokens(as(object, "TokenizedDocumentView"), transform),
        tagger(object)))
