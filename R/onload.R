.onLoad <- function(libname, pkgname) {
  .jinit(parameters="-Xmx2000m") # make sure the heap is big enough
  .jpackage(pkgname)
}
