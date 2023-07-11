# A base class for all tagged views.  Provides a slot for the tagger
# used and defines the tagger accessor method.

### BIZARRE PLACEMENT / FORWARD REFERENCE

# This is like a C++ forward reference...kinda.  See TokenizedView.R
# for the details/
setClass("Tagger", representation("VIRTUAL"))
setClass("TagSafe", representation("VIRTUAL"))

### CLASS DEF

# TaggedViews are TokenizedViews with a tagger.
setClass("TaggedView", 
  representation(tagger = "Tagger", "VIRTUAL"))

### GENERICS

setGeneric("tagger", function (object) standardGeneric("tagger"))

### ACCESSORS

setMethod("tagger", "TaggedView",
  function (object) return (object@tagger))
