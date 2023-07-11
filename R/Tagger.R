# A base class for all classes implementing the tagger interface.  

# tag takes an object representing a sequence of tokens (or if given
# an untokenized object, takes that object and applies the default
# tokenizer to it) and applies a tag to each token.  Extending class
# must provide a method definition of the form: 
#
# setMethod("tag", signature(object="character", tagger="MyTagger"),  ...  

### CLASS REF
### Defined, as below, in TaggedView.R for technical reasons.
#setClass("Tagger", representation("VIRTUAL"))

### GENERICS

# Below is in Tagged.R for load order
setGeneric("tag",
  function (object, tagger, tokenizer, index, ...) standardGeneric("tag"))


### TAG OVERLOADINGS

# The default tag behavior for characters is to assume that the
# character object contains a sequence of tokens when no tokenizer is
# given, but to tokenize the object first, if a tokenizer is
# specifically provided.  Note that we explicitly override the method
# for PlainTextDocument objects to provide tokenization in all cases.

# The method signature all extenders must implement
setMethod("tag", signature(object="character", tagger="Tagger",
                           tokenizer="missing", index="missing"),
  function (object, tagger, tokenizer, index)
    stop("tag must be implemented by derived class"))

# Calls the above method when a tokenizer is provided, after
# tokenizing
setMethod("tag", signature(object="character", tagger="Tagger",
                           tokenizer="Tokenizer", index="missing"),
  function (object, tagger, tokenizer, index)
      tag(tokenize(object, tokenizer), tagger))

# Now for non-character types

# Generate a TaggedDocumentView from a TokenizedDocumentView.
setMethod("tag", signature(object="TokenizedDocumentView",
                           tagger="Tagger",
                           tokenizer="missing",
                           index="missing"),
  function (object, tagger, tokenizer, index)
    TaggedDocumentView(object, tagger))

# Generate a TaggedCorpusView from a TokenizedCorpusView
setMethod("tag", signature(object="TokenizedCorpusView",
                           tagger="Tagger",
                           tokenizer="missing",
                           index="missing"),
  function (object, tagger, tokenizer, index)
    TaggedCorpusView(object, tagger))

# Generate a TaggedDocumentView from a TokenizedCorpusView + index
setMethod("tag", signature(object="TokenizedCorpusView",
                           tagger="Tagger",
                           tokenizer="missing",
                           index="numeric"),
  function (object, tagger, tokenizer, index)
    TaggedDocumentView(object[[index]], tagger))

# Generate a TaggedCorpus from a RSNLCorpus
setMethod("tag", signature(object="RSNLCorpus", 
                           tagger="Tagger",
                           index="missing"),
  function(object, tagger, tokenizer, index) 
    if (missing(tokenizer))
      tag(tokenize(object), tagger)
    else
      tag(tokenize(object, tokenizer), tagger))

# Generate a TaggedDocumentView from a RSNLCorpus + index.
setMethod("tag", signature(object="RSNLCorpus",
                           tagger="Tagger",
                           index="numeric"),
  function (object, tagger, tokenizer, index)
    if (missing(tokenizer))
      tag(tokenize(object), tagger, index=index)
    else
      tag(tokenize(object, tokenizer), tagger, index=index))
