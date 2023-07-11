# A generic tokenizer that splits text based on regular expressions

# Taken from NLTK
defaultPatternString <- "\\w+|[^\\w\\s]+"

### CLASS DEF

setClass("RegexTokenizer",
    representation(
        pattern     = "Pattern",
        matchToken  = "logical",
        "Tokenizer"
    )
#,
#    prototype(
#        pattern     = defaultPattern,
#        matchToken  = TRUE
#    )
)

### GENERICS
setGeneric("RegexTokenizer",
  function (pattern, matchToken=TRUE) standardGeneric("RegexTokenizer"))

setGeneric("pattern", function (object) standardGeneric("pattern"))

setGeneric("pattern<-", 
  function (object, value) standardGeneric("pattern<-"))

setGeneric("matchToken", function (object) standardGeneric("matchToken"))

setGeneric("matchToken<-", 
  function (object, value) standardGeneric("matchToken<-"))



## CONSTRUCTORS

# Use a method here to dispatch on both Pattern and simple
# character pattern types

# Creates a new regexTokenizer based on a compiled Pattern
setMethod("RegexTokenizer", "Pattern",
  function(pattern, matchToken=TRUE)
    return(new("RegexTokenizer", pattern=pattern, matchToken=matchToken)))

# A second constructor, for simple character-based patterns
setMethod("RegexTokenizer", "character", 
  function(pattern, matchToken=TRUE)
    return(new("RegexTokenizer", pattern=re.compile(pattern), 
               matchToken=matchToken)))

setMethod("RegexTokenizer", "missing",
  function(pattern, matchToken=TRUE)
    return(new("RegexTokenizer", pattern=re.compile(defaultPatternString), 
               matchToken=matchToken)))


### VALIDITY

# A valid regexTokenizer object should have a single regex pattern and a
# single matchToken value
setValidity("RegexTokenizer",
  function(object) {
    thisPattern <- pattern(object)
    thisMatchToken <- matchToken(object)

    if (length(thisPattern) != 1) {
        return("pattern must have length 1")
    }
    if (length(thisMatchToken) != 1) {
        return("matchToken must have length 1")
    }
    return(TRUE)
  }
)

### ACCESSORS

# Returns the pattern of the given regexTokenizer object
setMethod("pattern", "RegexTokenizer",
  function(object) return(object@pattern))

# Returns TRUE if the given regexTokenizer matches tokens, 
# FALSE if it matches separators.
setMethod("matchToken", "RegexTokenizer",
  function(object) return(object@matchToken))

### MODIFIERS

# Sets the pattern of the given regexTokenizer object to the given value
setMethod("pattern<-", signature(object="RegexTokenizer"), 
  function(object, value) {
      object@pattern <- value
      return(object)
  }
)

# Sets whether the given regexTokenizer matches tokens (TRUE) or separators
# (FALSE).
setMethod("matchToken<-", 
          signature(object="RegexTokenizer", value="logical"),
  function(object, value) {
      object@matchToken <- value
      return(object)
  }
)

### tokenize overriding(s)

# Tokenizes the given string with the given regexTokenizer object. Returns
# a character vector containing tokens.
setMethod("tokenize", signature(object="character", 
                      tokenizer="RegexTokenizer",
                      index="missing"), 
  function(object, tokenizer, index) {
    object <- paste(object, collapse=" ") # treat as one long string
    if (matchToken(tokenizer)) 
      re.findall(pattern(tokenizer), object)
    else
      re.split(pattern(tokenizer), object)
  })


### SHOW

setMethod("show", "RegexTokenizer",
  function (object)
    if (matchToken(object))
      cat("A token-matching regular expression tokenizer with pattern:\n",
          re.pattern(pattern(object)), "\n")
    else
      cat("A gap-matching regular expression tokenizer with pattern:\n",
          re.pattern(pattern(object)), "\n"))
