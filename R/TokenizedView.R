# A base class for all tokenized views.  Provides a slot for the
# tokenizer used and defines the tokenizer accessor method.

### BIZARRE PLACEMENT / FORWARD REFERENCE

# These are like C++ forward references...kinda.  Basically, because
# TokenizedView needs to know about Tokenizer, but the various
# basic tokenize method overridings need to know about
# TokenizedView's children, we define Tokenizer here, but define the
# methods in Tokenizer.R.  R loads TokenizedView.R, then
# TokenizedDocumentView, etc, and then Tokenizer.R and everything
# knows about everything it should.  Same deal with TokenTransform
setClass("Tokenizer", representation("VIRTUAL"))
setClass("TokenTransform", representation("VIRTUAL"))

### CLASS DEF

setClass("TokenizedView", 
  representation(tokenizer = "Tokenizer",  # The tokenizer used by the view
                 transforms = "list",    # TokenTransforms
                 "VIRTUAL"))

### GENERICS

setGeneric("tokenizer", function (object) standardGeneric("tokenizer"))

setGeneric("tokenTransforms",
  function (object) standardGeneric("tokenTransforms"))

setGeneric("tokenFilters", function (object)
  standardGeneric("tokenFilters"))

# All tokenized views should implement these methods

setGeneric("unique")

setGeneric("freqTable", function (object, ...) standardGeneric("freqTable"))

setGeneric("freq", 
  function (object, value, ...) standardGeneric("freq"))

setGeneric("has", function(object, value, ...) standardGeneric("has"))

### ACCESSORS

# Return the tokenizer used by this view
setMethod("tokenizer", "TokenizedView",
  function (object) return (object@tokenizer))

# Return the list of transforms applied to this view, in order
setMethod("tokenTransforms", "TokenizedView",
  function (object) object@transforms)

# Return the list of TokenFilter objects applied to this view
setMethod("tokenFilters", "TokenizedView",
  function (object)
    object@transforms[sapply(object@transforms, function (x) is(x, "TokenFilter"))])
