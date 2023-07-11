# Set globals that set the default storage behavior of views.

.keepComputed <- TRUE
.keepDocumentViews <- TRUE

setKeepComputedDefault <- function (keep) {
  if (! is.logical(keep))
    stop("keep must be a logical")

  assignInNamespace(".keepComputed", keep, "RSNL")
}

setKeepDocumentViewsDefault <- function (keep) {
  if (! is.logical(keep))
    stop("keep must be a logical")

  assignInNamespace(".keepDocumentViews", keep, "RSNL")
}

getKeepComputedDefault <- function () {
  .keepComputed
}

getKeepDocumentViewsDefault <- function () {
  .keepDocumentViews
}
