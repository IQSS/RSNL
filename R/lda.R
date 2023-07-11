# Class for representing fit latent dirichlet allocation models, along
# with some accessors and related methods.

setClass("lda",
  representation(alpha="numeric", beta="matrix", gamma="matrix",
                 num.terms="integer", num.topics="integer"))

setGeneric("topics", function (object, nwords=25) standardGeneric("topics"))
setGeneric("terms", function (object) standardGeneric("terms"))
setGeneric("num.terms", function (object) standardGeneric("num.terms"))
setGeneric("num.topics", function (object) standardGeneric("num.topics"))
setGeneric("get.alpha", function (object) standardGeneric("get.alpha"))
setGeneric("get.beta", function (object, log=TRUE) standardGeneric("get.beta"))
setGeneric("get.gamma", function (object) standardGeneric("get.gamma"))

setMethod("num.terms", "lda", function (object) object@num.terms)

setMethod("num.topics", "lda", function (object) object@num.topics)

setMethod("get.alpha", "lda", function (object) object@alpha)

setMethod("get.beta", "lda", function (object, log=TRUE) 
  if (log)
   object@beta
  else
    exp(object@beta))

setMethod("get.gamma", "lda", function (object) object@gamma)

setMethod("terms", signature("lda"),
  function (object) colnames(get.beta(object)))

setMethod("topics", signature("lda", "numeric"),
 function (object, nwords=min(num.terms(object), 25)) {
    sapply(1:dim(get.beta(object))[1], function (row)
      terms(object)[order(get.beta(object)[row,], decreasing=T)[1:nwords]])
  })
 
setMethod("show", "lda",
  function (object) {
    cat("A", num.topics(object), "topic by", num.terms(object),
    "term latent dirichlet allocation model fit\n")
  })

check_lda_args <- function(var.max.iter, var.convergence, em.max.iter,
                           em.convergence, estimate.alpha)
{
  if (length(var.max.iter) > 1 | length(var.convergence) > 1 |
      length(em.max.iter) > 1 | length(em.convergence) > 1 |
      length(estimate.alpha) > 1)
    stop("var.max.iter, var.convergence, em.max.iter, em.convergence",
         "and estimate.alpha must all be scalars")
  if (var.max.iter < -1)
    stop("var.max.iter must be positive or -1")
  if (var.convergence <= 0)
    stop("var.convergence must be positive")
  if (em.max.iter < 1)
    stop("em.max.iter must be at least 1")
  if (em.convergence <=0)
    stop("em.convergence must be positive")
}

# Takes a new corpus and generates variational dirichlet parameters
# for the documents in that corpus based on a fit lda model (a
# docsXterms gamma matrix).  The corpus can be represented as a
# DocuemntTermMatrix, TermDocumentMatrix, simple matrix (assumed to be
# document-term with terms as column names), TokenizedCorpusView,
# RSNLCorpus or Corpus (which are automatically tokenized with the
# default tokenizer), TokenizedDocumentView, or character vector (if
# one than more element is assume vector of tokens, else is tokenized
# with default tokenizer).
predict.lda <- function(object, newdata, var.max.iter=-1, 
                        var.convergence=1e-6, em.max.iter=100,
                        em.convergence=1e-4, estimate.alpha=TRUE) {
  if (!is.numeric(var.max.iter) | !is.numeric(var.convergence) |
      !is.numeric(em.max.iter) | !is.numeric(em.convergence))
    stop("var.max.iter, var.convergence, em.max.iter, em.convergence",
         "must be numeric")
  if (!is.logical(estimate.alpha))
    stop("estimate.alpha must be logical")
  check_lda_args(var.max.iter, var.convergence, em.max.iter,
                 em.convergence, estimate.alpha)

  # Convert to a document-term-matrix in sparse format
  if (is(newdata, "TokenizedCorpusView")) {
    newdata <- documentTokenMatrix(newdata)
  } else if (is(newdata, "RSNLCorpus")) {
    newdata <- documentTokenMatrix(tokenize(newdata))
  } else if (is(newdata, "Corpus")) {
    newdata <- documentTokenMatrix(tokenize(RSNLCorpus(newdata)))
  } else if (is(newdata, "DocumentTermMatrix")) {
    # do nothing
  } else if (is(newdata, "TermDocumentMatrix")) {
    newdata <- t(newdata)
  } else if (is(newdata, "matrix")) {
    newdata <- as.simple_triplet_matrix(newdata)
  } else if (is(newdata, "TokenizedDocumentView")) {
    newdata <- as.simple_triplet_matrix(t(as.matrix(freqTable(newdata))))
  } else if (is(newdata, "character")) {
    if (length(newdata) > 1)
      newdata <- as.simple_triplet_matrix(t(as.matrix(table(newdata))))
    else
      newdata <-
        as.simple_triplet_matrix(t(as.matrix(table(tokenize(newdata)))))
  } else {
    stop("predict.lda: newdata is not in a recognized format")
  }

  # Find new corpus term indices in model fit corpus
  terms <- terms(object)
  newterms <- colnames(newdata)
  m <- match(newterms, terms)

  # Zap words that weren't in model corpus
  index <- 1:length(newdata$j)
  index <- index[!is.na(m)]
  keep <- newdata$j %in% index
  newdata$i <- newdata$i[keep]
  newdata$j <- newdata$j[keep]
  newdata$v <- newdata$v[keep]

  # Line up with original matrix
  newdata$j <- m[newdata$j]
  newdata$ncol <- length(terms)
  newdata$dimnames[[2]] <- terms

  # Run inference
  nterms <- sapply(1:newdata$nrow, function(v) sum(newdata$i==v))
  res <- .Call("infer_lda", as.integer(newdata$v), as.integer(newdata$i),
    as.integer(newdata$j), as.integer(newdata$nrow), as.integer(nterms),
    as.integer(newdata$ncol), get.alpha(object), get.beta(object),
    as.integer(num.topics(object)), as.integer(var.max.iter), var.convergence,
    as.integer(em.max.iter), em.convergence, as.integer(estimate.alpha))
  res <- matrix(res, nrow=dim(newdata)[1], ncol=num.topics(fit))
  dimnames(res) <- list(docs=rownames(newdata), topics=NULL)
  res
}

# Methods for Latent Dirichlet Allocation model.  Base method works on
# document-token matrices but one can also call the method on
# token-document matrices, tokenized corpus views, RSNLCorpus objects,
# and tm corpus objects.  The corpus objects are tokenized with the
# default tokenizer before being fed to lda.

setGeneric("lda", function (object, alpha, k, var.max.iter=20, 
                            var.convergence=1e-6, em.max.iter=100,
                            em.convergence=1e-4, estimate.alpha=TRUE)
           standardGeneric("lda"))
setMethod("lda", signature("DocumentTermMatrix", "numeric", "numeric"),
  function (object, alpha, k, var.max.iter=20, var.convergence=1e-6, 
            em.max.iter=100, em.convergence=1e-4, estimate.alpha=TRUE) {

    check_lda_args(var.max.iter, var.convergence, em.max.iter,
                 em.convergence, estimate.alpha)

    nterms <- sapply(1:object$nrow, function(v) sum(object$i==v))
    res <- .Call("estimate_lda", as.integer(object$v), as.integer(object$i),
      as.integer(object$j), as.integer(object$nrow), as.integer(nterms), 
      as.integer(object$ncol), alpha, as.integer(k), as.integer(var.max.iter),
      var.convergence, as.integer(em.max.iter), em.convergence,
      as.integer(estimate.alpha))

    new("lda", num.terms=res[[1]], num.topics=res[[2]],
                alpha=res[[3]], 
                beta=matrix(res[[4]], nrow=k, ncol=object$ncol,
                            dimnames=list(topics=NULL, terms=colnames(object))),
                gamma=matrix(res[[5]], nrow=object$nrow, ncol=k,
                             dimnames=list(docs=rownames(object), topics=NULL)))
  })

setMethod("lda", signature("matrix"),
  function (object, alpha, k, var.max.iter=20, var.convergence=1e-6, 
            em.max.iter=100, em.convergence=1e-4, estimate.alpha=TRUE) {
    sparse <- as.simple_triplet_matrix(object)
    class(sparse) <- c("DocumentTermMatrix", "simple_triplet_matrix")
    lda(sparse, alpha, k, var.max.iter, var.convergence,
        em.max.iter, em.convergence, estimate.alpha)
  })

setMethod("lda", signature("TermDocumentMatrix"),
  function (object, alpha, k, var.max.iter=20, var.convergence=1e-6, 
            em.max.iter=100, em.convergence=1e-4, estimate.alpha=TRUE) {
    lda(t(object), alpha, k, var.max.iter, var.convergence,
        em.max.iter, em.convergence, estimate.alpha)
  })

setMethod("lda", signature("TokenizedCorpusView"),
  function (object, alpha, k, var.max.iter=20, var.convergence=1e-6, 
            em.max.iter=100, em.convergence=1e-4, estimate.alpha=TRUE) {
    lda(documentTokenMatrix(object), alpha, k, var.max.iter,
        var.convergence, em.max.iter, em.convergence, estimate.alpha)
  })

setMethod("lda", signature("RSNLCorpus"),
  function (object, alpha, k, var.max.iter=20, var.convergence=1e-6, 
            em.max.iter=100, em.convergence=1e-4, estimate.alpha=TRUE) {
    lda(tokenize(object), alpha, k, var.max.iter,
        var.convergence, em.max.iter, em.convergence, estimate.alpha)
  })

setMethod("lda", signature("Corpus"),
  function (object, alpha, k, var.max.iter=20, var.convergence=1e-6, 
            em.max.iter=100, em.convergence=1e-4, estimate.alpha=TRUE) {
    lda(RSNLCorpus(object), alpha, k, var.max.iter,
        var.convergence, em.max.iter, em.convergence, estimate.alpha)
  })
