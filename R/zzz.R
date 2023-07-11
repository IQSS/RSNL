.onUnload <- function(libpath) {
  library.dynam.unload("RSNL")
}

# This stuff is defined in tm but gets clobbered or not exported correctly.
# Redefining here for the time being so our vignette code will work.
# XXX remove when no longer needed.

simple_triplet_matrix <-
function(i, j, v, nrow = max(i), ncol = max(j), dimnames = NULL)
{
    structure(list(i = i, j = j, v = v, nrow = nrow, ncol = ncol,
                   dimnames = dimnames),
              class = "simple_triplet_matrix")
}

as.simple_triplet_matrix <-
function(x)
    UseMethod("as.simple_triplet_matrix")

as.simple_triplet_matrix.simple_triplet_matrix <- identity

as.simple_triplet_matrix.matrix <-
function(x)
{
    if(!prod(dim(x)))
        return(simple_triplet_matrix(integer(), integer(), c(x),
                                     nrow = nrow(x), ncol = ncol(x),
                                     dimnames = dimnames(x)))
    ind <- which(x != vector(typeof(x), 1L), arr.ind = TRUE)
    dimnames(ind) <- NULL
    simple_triplet_matrix(ind[, 1L], ind[, 2L], x[ind],
                          nrow = nrow(x), ncol = ncol(x),
                          dimnames = dimnames(x))
}

as.matrix.simple_triplet_matrix <-
function(x, ...)
{
    nr <- x$nrow
    nc <- x$ncol
    y <- matrix(vector(typeof(x$v), nr * nc), nr, nc) 
    y[cbind(x$i, x$j)] <- x$v 
    dimnames(y) <- x$dimnames
    y   
}
