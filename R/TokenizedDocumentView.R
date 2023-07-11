# A view of an RSNLDocument that has been tokenized in some way.
# Provides the user with easy access to a token vector, frequency
# table, and unique tokens.

### CLASS DEF

# TokenizedDocumentView - A view of a document as a sequence of tokens.
setClass("TokenizedDocumentView",
    representation("DocumentView", "TokenizedView",
      tokens          =   reftype("character"),
      tokenFreqTable  =   reftype("table"),
      uniqueTokens    =   reftype("character")
    )
)

### VALIDITY

# XXX TODO

### GENERICS

setGeneric("tokens", function (object) standardGeneric("tokens"))

### CONSTRUCTION

# Constructs a new TokenizedDocumentView
# Not part of the public interface, but should always be used within
# the library to create TDVs
TokenizedDocumentView <- function(corpus, tokenizer, index,
                                  transforms=list()) {
  tokens <- new("NULLcharacter")
  tokenFreqTable <- new("NULLtable")
  uniqueTokens <- new("NULLcharacter")

  res <- new("TokenizedDocumentView",
      corpus=corpus, index=index, tokenizer=tokenizer, 
      keepComputed = .keepComputed,
      transforms=transforms, tokens=tokens,
      tokenFreqTable=tokenFreqTable, uniqueTokens=uniqueTokens)

  # This dv will get notified whenever corpus changes in any way.
  # Should only respond if index passed in updateView call.
  registerView(corpus, res)
  return(res)
}

### CALLBACK method

setMethod("clearComputed", "TokenizedDocumentView",
  function(object) {
    ref(object, "tokens") <- new("NULLcharacter")
    ref(object, "tokenFreqTable") <- new("NULLtable")
    ref(object, "uniqueTokens") <- new("NULLcharacter")
  })

# Set all values to null for future lazy reloading.
# Only respond if our doc got changed.
setMethod("updateView", 
          signature("TokenizedDocumentView", "ViewableObject"),
  function (view, viewable, index=NULL)
    if ((!is.null(index)) && (index(view) %in% index)) clearComputed(view))
    
### ACCESSORS

# Returns the tokens of this TokenizedDocumentView
# XXX assumes a character-extending document type.
setMethod("tokens", "TokenizedDocumentView",
  function(object)
    if (is(ref(object, "tokens"), "NULLcharacter")) {
      tokens <- tokenize(document(object), tokenizer(object))
      for (transform in object@transforms)
        tokens <- transformTokens(tokens, transform)
      if (keepComputed(object))
        (ref(object, "tokens") <- tokens)
      else
        tokens
    } else
      ref(object, "tokens"))

# Returns the tokens at the given indices
setMethod("[", signature(x="TokenizedDocumentView", j="missing"), 
  function(x, i, j, drop) tokens(x)[i])

# Returns the frequency table of tokens
setMethod("freqTable", "TokenizedDocumentView", 
  function(object, ...) {
    # lazy load
    if (is(ref(object, "tokenFreqTable"), "NULLtable")) {
      if (keepComputed(object))
        (ref(object, "tokenFreqTable") <- table(tokens(object)))
      else
        table(tokens(object))
    } else
      ref(object, "tokenFreqTable")
  }
)

# Returns the unique tokens
setMethod("unique", "TokenizedDocumentView", 
  function(x, incomparables="missing") {
    # lazy load
    if (is(ref(x, "uniqueTokens"), "NULLcharacter")) {
      uniqueTokens <- row.names(ref(x, "tokenFreqTable"))
      if (is.null(uniqueTokens))
        uniqueTokens <- unique(tokens(x))
      if (keepComputed(x))
        (ref(x, "uniqueTokens") <- uniqueTokens)
      else
        uniqueTokens
    } else
      ref(x, "uniqueTokens")
  }
)

# Returns TRUE if this contains the given token
setMethod("has", signature(object="TokenizedDocumentView", 
                           value="character"),
  function(object, value, ...) 
    return(value %in% unique(object)))


# Returns the frequency of the given token
setMethod("freq", signature(object="TokenizedDocumentView", 
                            value="character"),
  function(object, value, ...)
    if (has(object, value))
        return(freqTable(object)[[value]])
    else
      return(0))

### UTILS

# Prints some information about this view to the screen
setMethod("show", "TokenizedDocumentView",
  function(object) {
    name <- ID(document(object))
    nTokens <- length(tokens(object))
    nUniqueTokens <- length(unique(object))

    str <- paste("A tokenized document view of", name, "with", nTokens, 
                 "total tokens and", nUniqueTokens, "unique tokens\n")
    cat(str)
  }
)
