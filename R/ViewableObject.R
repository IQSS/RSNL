# A base class for "viewable" objects which handles the assignment of
# views to this object and sets up a callback mechanism.
# ViewableObjects contains a hashed environment called views which
# contains (pointers to) the environments underlying particular views.
# Views are assumed to make use of the RClass framework and to hold
# all of their persistent info in the .refslots environment slot.
# When a view registers with a ViewableObject, the object copies (a
# pointer to) the .refslots environment of the view into its views
# environment if it does not already contain (a pointer to) the view's
# .refslots environment.  Enviornments are keyed by string
# representations of their memory addresses within the views hash/env.
# Registering a view with the ViewableObject also registers the view's
# .refslots environment with a finalizer that will remove the
# .refslots reference from the ViewableObject's views hash when the
# .refslots environment is garbage collected (this will only happen
# when there are no more copies of the view alive).
#
# !!! This is complicated but the key thing to remember is that
# extending classes must place all information that will get used by
# the updateView method for that class in reference slots (in the
# .refslots object, to be precise) or updateViews calls will not
# propogate correctly.


### CLASSDEF

setClass("ViewableObject", representation(views="environment"))

### GENERICS

setGeneric("registerView", 
  function (object, view) standardGeneric("registerView"))

# Should be implemented by views, not viewables
setGeneric("updateView", function (view, viewable, ...) 
  standardGeneric("updateView"))

setGeneric("updateViews",
  function (object, ...) standardGeneric("updateViews"))

setGeneric("viewRemover", 
  function (object) standardGeneric("viewRemover"))

### Construction

# Exteding classes should use this constructor to build objects.  For
# example:
# setClass("myclass", representation("ViewableObject",
#                                    x=reftype("numeric")))
# myclass <- function (x) new("myclass", ViewableObject(), x=x)
ViewableObject <- 
  function () new("ViewableObject", views = new.env(hash=TRUE))

### CLEANUP FUNCTION FACTORY

# This method returns a closure for removing views from a viewable
# object.
setMethod("viewRemover", "ViewableObject",
  function (object) {
    function (x) {
      id <- .Call("viewable_env_addr", x)
      if (! exists(id, object@views, inherits=FALSE))
        stop(paste("View", x, "of environment", env, "is not a view",
                   "of this object.  Aborting finalization."))
      rm(list=id, pos=object@views)
    }
  })
    

### MODIFERS

setMethod("registerView", signature("ViewableObject", "RObject"),
  function (object, view) {
    env <- view@.refslots
    id <- .Call("viewable_env_addr", env)
    if (! exists(id, object@views, inherits=FALSE)) {
      pnt <- .Call("viewable_get_pointer", env)
      # We make hash entries of the form
      #id => (env = extptr(view@.refslots), class = class(view))
      # where class(view) returns the views classname as a string
      assign(id, list(env=pnt, class=class(view)), 
             object@views, inherits=FALSE)
      reg.finalizer(env, viewRemover(object))
    }
  })

### CALLBACK mechanism

# This method calls the updateView method of all registered views.
# Probably doesn't need to be overridden for extending classes.
# Extending classes need to call this method whenever they get
# modified.
setMethod("updateViews", "ViewableObject",
  function (object, ...) lapply(as.list(object@views), function (x)  {
    tmpview <- new(x$class)
    tmpview@.refslots <- .Call("viewable_get_object", x$env)
    updateView(tmpview, object, ...)
  }))
