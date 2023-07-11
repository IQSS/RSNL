# A simple wrapper around the stemmer provided by rstem.

### CLASSDEF

setClass("SnowballStemmer",
  representation("Stemmer", "TagSafe", language = "character"))

### CONSTRUCTOR

SnowballStemmer <- function (language = "english") {
  if (! (language %in% getStemLanguages()))
    stop(paste(language, "is not a valid Snowball stemmer language."))
  new("SnowballStemmer", language=language)
}

### ACCESSORS

# Get the language of this stemmer
setMethod("language", "SnowballStemmer", function (object) object@language)

### stem overriding

# Stems the given string, or vector of tokens, but always break a
# TextDocument down into its tokens first.
setMethod("stem", signature(object="character", 
                            stemmer="SnowballStemmer",
                            tokenizer="missing",
                            index="missing"), 
  function(object, stemmer, tokenizer, index) 
    if (is(object, "TextDocument"))
      wordStem(tokenize(object), language(stemmer))
    else
      wordStem(object, language(stemmer)))

# Specialization for Tagged objects
setMethod("stem", signature(object="Tagged", 
                            stemmer="SnowballStemmer",
                            tokenizer="missing",
                            index="missing"), 
  function(object, stemmer, tokenizer, index) {
    tags <- label(object)
    Tagged(wordStem(object, language(stemmer)), tags)
  })


### SHOW

setMethod("show", "SnowballStemmer",
  function (object)
    cat("A Snowball stemmer for the ", language(object), " language.\n"))
