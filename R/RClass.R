# Total kludge.  Suddenly necessary for R2.9.0+
# XXX Should find cleaner workaround.  Basically, new no longer plays
# nicely with classes extending classRepresentation, breaking our
# setup.  This overwrites new in the methods package to get around the
# problem, but is pretty bad form.  Seems backwards compatible at
# least.
unlockBinding("new", environment(new))
assign("new", function (Class, ...)
{
    if (is(Class, "RClassRepresentation"))
      ClassDef <- Class
    else if (methods:::.identC(class(Class), "classRepresentation")) 
      ClassDef <- Class
    else ClassDef <- getClass(Class, where = topenv(parent.frame()))
    value <- .Call("R_do_new_object", ClassDef, PACKAGE = "base")
    initialize(value, ...)
}, environment(new))

# Extend classRepresentation to provide representation class for RObjects
setClass("RClassRepresentation",
  representation=representation(refslots = "list"),
  contains="classRepresentation")

# Set up a placeholder class for reference types.
setClass("Reference", "VIRTUAL")

# Define an RObject class.  All S4 objects with "reference slots"
# extend this class, although this is handled under the hood by a
# redefinition of setClass.
setClass("RObject", 
  representation(.refslots = "environment", "VIRTUAL"))

# Setup baseline display for RObjects
setMethod("show", "RObject",
  function(object) {
    clDef <- getClass(cl <- class(object), .Force = TRUE)
    # XXX classLabel replacement...not pressing
    cat("An object of class ", cl, "\n", sep = "")
    slots <- setdiff(slotNames(clDef), ".refslots")
    refslots <- names(clDef@refslots)
    slots <- setdiff(slots, refslots)
    for (what in slots) {
      cat("Slot \"", what, "\":\n", sep = "")
      print(slot(object, what))
      cat("\n")
    }
    for (what in refslots) {
      cat("Reference Slot \"", what, "\":\n", sep = "")
      print(get(what, object@.refslots))
      cat("\n")
    }
  }
)

# A default initialize method for RObjects.  This behaves just like
# the default initialize method except it handles reference slot
# initialization transparently.  Note that all reference
# initialization is done by pure copy:
# setClass("A", representation(a=reftype("numeric")))
# a  <- new("A", a=42)
# b  <- new("A", a)
# ref(b, "a") <- 54
# ref(a, "a") == ref(b, "a") ==> FALSE

setMethod("initialize", "RObject",
  function (.Object, ...) {
    args <- list(...)
    type <- class(.Object)
    type.rslots <- getClass(type)@refslots
    slotnames <- names(type.rslots)
    # Figure out what to pass on and what to keep
    pidx <- sapply(methods:::allNames(args), function (x) ! x %in% slotnames)
    if (length(pidx) > 0)
      passon <- args[pidx]
    else
      passon <- list()

    kidx <-  sapply(methods:::allNames(args), function (x) x %in% slotnames)
    if (length(kidx) > 0)
      keep <- args[kidx]
    else
      keep <- list()

    # Build up a baseline initialize call and execute it
    f <- selectMethod("initialize", "ANY")
    mycall <- as.call(c(f, .Object, passon))
    obj <- eval(mycall)

    # Grab any preinitialized (by superclass object) reference slots
    preinit <- as.list(obj@.refslots)

    # Create a brand new environment
    obj@.refslots <- new.env()

    # Initialize reference slots
    for (name in slotnames) {
      if (name %in% names(keep)) 
        assign(name, keep[[name]], obj@.refslots)
      else if (name %in% names(preinit))
        assign(name, preinit[[name]], obj@.refslots)
      else
        #assign(name, new(type.rslots[[name]]), obj@.refslots)
        assign(name, NULL, obj@.refslots) # XXX what we want?

      ### 2.9.0+ throws error if attempt to new() a virtual class.  So
      # now we don't bother new()ing stuff, and just leave it null.
      # Not sure this is correct.  Seems to work fine...
    }
    obj
  }
)

# A method to clone RObjects
setGeneric("clone", function (x) standardGeneric("clone"))
setMethod("clone", "RObject",
  function (x) new(class(x), x))

# A function for passing reftypes to representation().  Works like
# this: representation("Foo", a="Bar", b=reftype("Baz"), ...)
setGeneric("reftype", function (type) standardGeneric("reftype"))
setMethod("reftype", "character",
  function (type) {
    if (! isClass(type))
      stop(gettextf("%s is an invalid class type", type))
    rtype <- paste("Reference", type, sep=".")
    if (! isClass(rtype))
      setClass(rtype, contains="Reference")
    rtype
  }
)

# Return a list of named slots that are reference slots with the
# underlying types.  For example
# list(a="numeric", "Reference.numeric", b="Reference.numeric")
# ->
# list(b="numeric")
referenceSlots <- function (repr)
  lapply(grep("^Reference\\.", repr[nzchar(names(repr))], value=T), 
    function (x) sub("^Reference\\.", "", x))

# This function is a lot like setClass, except that it handles
# inheritance of reference slots and sets up the reference slot
# environment for the created object, if needed.
setClass <- function (Class, representation=list(), prototype=NULL,
                      contains=character(), 
                      validity=NULL, access=list(), 
                      where = topenv(parent.frame()),
                      version = methods:::.newExternalptr(),
                      sealed = FALSE, package = getPackageName(where))
{
  refslots <- referenceSlots(representation)
  if (length(refslots) > 0)
    contains = c(contains, "RObject")

  # Generate the basic class definition
  #methods:::setClass
  methods:::setClass(Class=Class, representation=representation,
                     prototype=prototype,  contains=contains,
                     validity=validity, where=where, sealed=FALSE,
                     package=package)

  # Grab the now-existant class definition and get supers list
  classDef <- getClassDef(Class, where, package)
  superClasses <- methods:::allNames(classDef@contains)

  if ("RObject" %in% superClasses) { # is this a RObject
    # For now, refuse to create an RObject that also extends a basic
    # class.  XXX Can get rid of this restriction once we fix init bug
    if (any(sapply(superClasses,
                   function (x) any(x %in% methods:::.BasicClasses))))
      stop("RClass currently contains no support for RObjects that extend basic class types.")

    # Inherit refslots
    lapply(superClasses, function (x) {
      superClassDef <- getClassDef(x)
      if (is(superClassDef, "RClassRepresentation")) {
        slots <- superClassDef@refslots
        refslots <<- c(refslots, slots[setdiff(names(slots), names(refslots))])
      }
    })

    # Upgrade to RClassRepresentation and rest the class
    classDef <- new("RClassRepresentation", slots=classDef@slots,
              contains=classDef@contains, virtual=classDef@virtual,
              prototype=classDef@prototype, validity=classDef@validity,
              access=classDef@access, className=classDef@className,
              package=classDef@package, subclasses=classDef@subclasses,
              versionKey=classDef@versionKey, sealed=sealed,
              refslots=refslots)
    resetClass(Class, classDef, where)
  }
  Class
}

# Provide access to reference fields

setGeneric("ref", function (x, name) standardGeneric("ref"))
setGeneric("ref<-", function (x, name, value) standardGeneric("ref<-"))

setMethod("ref", "RObject",
  function (x, name) get(name, x@.refslots, inherits=FALSE))
    
setMethod("ref<-", "RObject",
  function (x, name, value) {
    otype <- class(x)
    stype <- (getClass(class(x))@refslots[name])[[1]]

    # slot check
    if (is.null(stype))
      stop(paste("Objects of type", class(x), 
        "have no reference slot named", name))

    # Type check
    if (! is(value, stype))
      stop(paste("Reference slot", name, "holds objects of type", stype))

    assign(name, value, x@.refslots, inherits=FALSE)
    x
  }
)
