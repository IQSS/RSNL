# A view of a CorpusRef that has been tokenized in some way.  Provides
# the user with easy access to a DocumentTermMatrix, unique tokens, and the
# like.

### CLASS DEF

setClass("TokenizedCorpusView",
    representation(
        "CorpusView",
        "TokenizedView",
        tokenFreqTable          =   reftype("table"),
        uniqueTokens            =   reftype("character"),
        documentTokenMatrix     =   reftype("DocumentTermMatrix")
    )
)

### VALIDITY

# XXX TODO

### Various functions

# Make document-token matrix
makeDocumentTokenMatrix <- function(termfreqlist, uterms, docnames,
                                    sorted=FALSE) {
  if (! sorted)
    uterms <- sort(uterms)
  i <- lapply(termfreqlist, function (x) match(names(x), uterms)[x>0])
  i <- unlist(i, use.names=F)
  j <- rep(seq_along(termfreqlist), sapply(termfreqlist, length))
  v <- as.numeric(unlist(termfreqlist, use.names=FALSE))
  len <- length(termfreqlist)
  rm(termfreqlist)
  tdm <- structure(list(i = i, j = j, v = v, nrow = length(uterms), ncol = len,
                 dimnames = list(Terms = uterms, 
                                 Docs = docnames),
                 Weighting = c(weightTf@Name, weightTf@Acronym)),
            class = c("TermDocumentMatrix", "simple_triplet_matrix"))
  t(tdm)
}

### GENERICS

setGeneric("documentTokenMatrix",
  function (object, weight, ...) standardGeneric("documentTokenMatrix"))

### CONSTRUCTION

# Constructs a new TokenizedCorpusView from given corpus, tokenizer,
# and optional list of token transforms
# This constructor is not intended to be part of the external api and
# users will generate TokenizedCorpusViews with the tokenize,
# filterTokens, transformTokens and similar  methods.
# Always use this function to generate TCVs within the library.
TokenizedCorpusView <- function(corpus, tokenizer, transforms=list(),
                                documentFilters=list()) {

  documentViews <- new("NULLlist")
  tokenFreqTable <- new("NULLtable")
  uniqueTokens <- new("NULLcharacter")
  documentTokenMatrix <- new("NULLDocumentTermMatrix")

  res<- new("TokenizedCorpusView", corpus=corpus,
            keepComputed = .keepComputed,
            tokenizer=tokenizer,
            transforms=transforms,
            documentFilters=documentFilters,
            documentViews=documentViews,
            keepDocumentViews = .keepDocumentViews,
            tokenFreqTable=tokenFreqTable,
            uniqueTokens = uniqueTokens,
            documentTokenMatrix=documentTokenMatrix)
  registerView(corpus, res)
  res
}

### CALLBACK methods

# Clear computed values
# NOTE: we don't want to null out documentViews here because they
# update dynamically themselves.
setMethod("clearComputed", "TokenizedCorpusView",
  function (object) {
    ref(object, "tokenFreqTable") <- new("NULLtable")
    ref(object, "uniqueTokens") <- new("NULLcharacter")
    ref(object, "documentTokenMatrix") <- new("NULLDocumentTermMatrix")
  })

# Maybe could optimize to make less blunt
setMethod("updateView",
          signature("TokenizedCorpusView", "ViewableObject"),
  function (view, viewable, ...) clearComputed(view))

### ACCESSORS / MODIFIERS (lazy loading)

# A helper method for generating a list of all tokenized document
# views in a corpus.  Used by various specializations of
# documentViews.
genTokDVs <- function (object, proto) {
  corpus <- corpus(object)

  # clone and set index
  tdvs <- sapply(1:length(corpus), function (index) {
    tdv <- clone(proto)
    ref(tdv, "index") <- index
    # need to register view cause not using constructor
    registerView(corpus, tdv)
    tdv
  })

  # Name the tdvs
  # XXX this is amazingly slow...  Takes something like 2 seconds
  # for 500 docs on x5!
  names(tdvs) <- sapply(corpus(object), ID)

  # Filter the tdvs
  for (filter in object@documentFilters)
    tdvs <- filterDocuments(tdvs, filter)

  if (keepDocumentViews(object))
    (ref(object, "documentViews") <- tdvs)
  else
    tdvs
}

# Returns the documentViews of the underlying documents
setMethod("documentViews", "TokenizedCorpusView", 
  function (object) {
    if (is(ref(object, "documentViews"), "NULLlist")) {
      # Cronstruct a prototype dv
      proto <- TokenizedDocumentView(corpus(object), tokenizer(object), 1,
                                     object@transforms)
      genTokDVs(object, proto)
    } else
      ref(object, "documentViews")
  })

# Returns the unique tokens in this view
setMethod("unique", "TokenizedCorpusView",
  function (x, incomparables="missing", ...)
    if (is(ref(x, "uniqueTokens"), "NULLcharacter")) {
        
      result <- unique(unlist(lapply(documentViews(x), unique)))
      #result <- unique(unlist(lapply(documentViews(x), tokens)))
      if (keepComputed(x))
        (ref(x, "uniqueTokens") <- result)
      else
        result
    } else
      ref(x, "uniqueTokens"))

# Returns a frequency table of tokens in the corpus, across all docs
setMethod("freqTable", "TokenizedCorpusView",
  function (object, ...)
    if (is(ref(object, "tokenFreqTable"), "NULLtable")) {
      result <- table(unlist(lapply(documentViews(object), tokens),
                             use.names=F))
      if (keepComputed(object))
        (ref(object, "tokenFreqTable") <- result)
      else
        result
    } else
      ref(object, "tokenFreqTable"))

# Returns TRUE if this contains the given token
setMethod("has", signature(object="TokenizedCorpusView", value="character"),
  function(object, value, ...) return(value %in% unique(object)))

# Returns the total frequency of the given token within the corpus
setMethod("freq", signature(object="TokenizedCorpusView",
                            value="character"),
  function (object, value, ...)
    if (has(object, value))
      return(freqTable(object)[[value]])
    else
      return (0))

# Returns the document-token frequency matrix of this view, optionally
# weighted.
setMethod("documentTokenMatrix", 
          signature(object = "TokenizedCorpusView", 
                    weight = "missing"),
  function(object, weight=weightTf)
    if (is(ref(object, "documentTokenMatrix"), "NULLDocumentTermMatrix")) {
      # XXX what gets ignored normally, but useful for extending classes
      result <- makeDocumentTokenMatrix(lapply(documentViews(object),
                                               freqTable, what="Tokens"),
                                        unique(object, what="Tokens"),
                                        names(documentViews(object)))
      if (keepComputed(object))
        (ref(object, "documentTokenMatrix") <- result)
      else
        result
    } else
      ref(object, "documentTokenMatrix"))

setMethod("documentTokenMatrix", 
          signature(object = "TokenizedCorpusView", 
                    weight = "WeightFunction"),
  function(object, weight=weightTf) 
    weight(documentTokenMatrix(object)))

# UTILITES

# prints out the corpus id, total number of tokens, and total number of 
# unique tokens
setMethod("show", "TokenizedCorpusView",
  function(object) {
    nUnique <- length(unique(object))
    nTotal <- sum(documentTokenMatrix(object)$v) # XXX very impl dep

    str <- paste("A tokenized corpus view with", nTotal, 
                 "total tokens and", nUnique, "unique tokens\n")
    cat(str)
  }
)
