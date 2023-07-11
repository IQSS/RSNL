# Tokenizes a string object based on offsets.  Offsets are integer
# counts into the character string.  For example, the set of offsets
# [1, 7, 22] would split a string up into the ranges 1-6, 7-21, and
# 22-end.

### CLASS DEF

setClass("OffsetTokenizer",
    representation(
        offset = "integer",
        "Tokenizer"
    )
)

### VALIDITY

"isValidOffsetTokenizer" <-
function(object) {
    if (all(sort(object@offset) != object@offset))
      return("Offsets must be strictly increasing")
    return(TRUE)
}
setValidity("OffsetTokenizer", isValidOffsetTokenizer)

### GENERICS

setGeneric("offsets", function (object) standardGeneric("offsets"))

setGeneric("offsets<-",
  function (object, value) standardGeneric("offsets<-"))

### CONSTRUCTION

# Creates a new OffsetTokenizer
"OffsetTokenizer" <-
function(offset) {
    return(new("OffsetTokenizer", offset=as.integer(offset)))
}

### ACCESSORS

setMethod("offsets", "OffsetTokenizer",
  function(object) {
    return(object@offset)
  }
)

### MODIFIERS

setMethod("offsets<-", signature(object="OffsetTokenizer"),
  function(object, value) {
    if (all(sort(value) != value))
      stop("Offset vector must be strictly increasing")

    object@offset <- value
    object
  }
)

### overridden tokenize method

# Tokenizes the given string with an OffsetTokenizer, returning a
# character vector.
setMethod("tokenize", signature(object="character", 
                                tokenizer="OffsetTokenizer",
                                index="missing"),
  function(object, tokenizer, index) {

    offsets <- offsets(tokenizer)
    if (any(offsets[length(offsets)] > nchar(object)))
      stop("Offsets cannot extend past end of string")

    unlist(lapply(object, function (x) {
      offsets <- cbind(offsets, c(offsets[-1] - 1, nchar(x)))
      apply(offsets, 1, function(offset) {
        substr(x, offset[1], offset[2])})
    }))
  }
)

### SHOW

setMethod("show", "OffsetTokenizer",
  function (object)
    cat("An offset tokenizer using offset sequence:\n",
        offsets(object), "\n"))
