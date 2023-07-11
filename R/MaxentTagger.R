# An interface to the Stanford NLP group's maximum entropy part of
# speech tagger.  The Stanford MaxentTagger object is largely static
# and, one you've loaded a given trained tagger, you can't load
# another one in the same java session.  Therefore, the RSNL interface
# works the same way.  Once you've instantiated a single tagger you
# can't create others that use a different language.

# A static (final) package variable, via closure.  The generated
# function returns NULL until a language is given, after which it will
# always return the initial language, no matter what the input.
.maxentTagger.language.factory <- function () {
  language <- NULL

  function (val=NULL) {
    cur <- language
    if (is.null(language) & !is.null(val))
      language <<- val
    cur
  }
}
.maxentTagger.language <- .maxentTagger.language.factory()
      

### CLASS DEF

setClass("MaxentTagger",
  representation("Tagger"))
 
### GENERICS

setGeneric("language", function (object) standardGeneric("language"))

### CONSTRUCTION

# This initializes the MaxentTagger class in the underlying java code
# if this is the first call to the constructor, otherwise it
# ensures that the user is using the same language as before.  If no
# tagger has been initialized the MaxentTagger() will generate a
# tagger for English, after that it will assume no-argument calls
# request taggers for the initialized language.
MaxentTagger <- function (language = NULL) {
  if (is.null(language)) {
    if (is.null(.maxentTagger.language()))
      language <- "english"
    else
      language <- .maxentTagger.language()
  } else
    language <- tolower(language)

  # Note the first call to .maxentTagger.language() sets the language
  # if it isn't already.
  if (is.null(.maxentTagger.language(language))) {
    filename <- system.file(paste("taggers/pos/", 
                                  paste(language, "tagger", sep='.'),
                                  sep='/'),
                            package="RSNL")
    props <- paste(filename, "props", sep='.')
    if (filename == "")
      stop("No tagger for ", language)

    .jcall("MaxentTaggerBridge", "V", "init", filename, props)
  } else if (.maxentTagger.language() != language)
    stop("The MaxentTagger engine was already initialized for ",
         .maxentTagger.language(), " and cannot be reinitialized ",
         "within a single R session.")

  new("MaxentTagger")
}

### ACCESSORS
setMethod("language", "MaxentTagger",
  function (object) .maxentTagger.language())

### tag overriding

# If object is a character vector of length one:
#   applies PTBTokenizer and tags
# If object is a TextDocument
#   Collapses to single string (if necessary), separating with spaces
#   applies PTBTokenizer and tags
# If object is non-TextDocument character vector w/length > 1
#   Treats is item as a token and applies tags
setMethod("tag", signature(object="character",
                           tagger="MaxentTagger",
                           tokenizer="missing",
                           index="missing"),
  function(object, tagger, tokenizer, index) {
    if (is(object, "TextDocument"))
      object <- paste(object, collapse=" ")
    if (length(object) == 1)
      .jcall("MaxentTaggerBridge", "V", "tagString", object)
    else
      .jcall("MaxentTaggerBridge", "V", "tagTokens", object)

    tokens <- .jcall("MaxentTaggerBridge", "[S", "getTokens")
    tags <- .jcall("MaxentTaggerBridge", "[S", "getTags")

    Tagged(tokens, tags)
  })

# Explicitly override this to tokenize in place.  Collapses any
# character vector w/length > 1 to a single string, separating with
# spaces.
setMethod("tag", signature(object="character", tagger="MaxentTagger",
                           tokenizer="PTBTokenizer", index="missing"),
  function(object, tagger, tokenizer, index) {
    if (length(object) > 1)
      object <- paste(object, collapse=" ")

    .jcall("MaxentTaggerBridge", "V", "tagString", object)
    tokens <- .jcall("MaxentTaggerBridge", "[S", "getTokens")
    tags <- .jcall("MaxentTaggerBridge", "[S", "getTags")

    Tagged(tokens, tags)
  })

### SHOW

setMethod("show", "MaxentTagger",
  function (object)
    cat("A Maximum Entropy part of speech tagger for ", 
        language(object), ".\n"))
