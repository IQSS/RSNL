# tokenizer - a base class for tokenizer classes

### CLASS DEF

### Defined, as below, in TokenizedView.R for technical reasons.
#setClass("Tokenizer", representation("VIRTUAL"))

### GENERICS

setGeneric("tokenize",
  function(object, tokenizer, index) standardGeneric("tokenize"))

### METHODS

# The tokenize method overridden for RSNLCorpus object
# types with generic Tokenizer objects.  Particular tokenizers
# override tokenize for character objects (and themselves); these
# overridden methods appear in the same file as the given tokenizer
# class definition.

# The character tokenizer takes a character vector of any length and
# returns the character vector that results from applying the
# tokenizer to each character object in the input vector and
# flattening the resulting list.  Thus, it retains no information
# about the structure of the input vector.  This does not mean that
# tokenizing a vector of characters will always produce the same
# output as tokenizing a single string of all those vectors cat'ed
# together; for example an offset tokenizer will count offsets from
# the beginning of each character element.

# Generate a TokenizedDocumentView from a RSNLCorpus + index.
setMethod("tokenize", signature(object="RSNLCorpus",
                                tokenizer="Tokenizer",
                                index="numeric"),
  function (object, tokenizer, index)
    TokenizedDocumentView(corpus=object, index=index, 
                          tokenizer=tokenizer))


# Generate a TokenizedCorpusView from a RSNLCorpus  Note that
# the actual tokenization is delayed via a lazy loading mechanism.
setMethod("tokenize", signature(object="RSNLCorpus", tokenizer="Tokenizer",
                                index="missing"),
  function(object, tokenizer, index) TokenizedCorpusView(object, tokenizer))
