\name{lda-class}
\docType{class}
\alias{lda-class}
\alias{get.alpha,lda-method}
\alias{get.alpha}
\alias{get.beta,lda-method}
\alias{get.beta}
\alias{get.gamma,lda-method}
\alias{get.gamma}
\alias{num.terms,lda-method}
\alias{num.terms}
\alias{num.topics,lda-method}
\alias{num.topics}
\alias{show,lda-method}
\alias{terms,lda-method}
\alias{terms}
\alias{topics,lda-method}
\alias{topics}

\title{Class "lda"}

\description{A fitted Latent Dirichlet Allocation model.}

\section{Objects from the Class}{
Objects are created by calls to the \code{\link{lda}} method.}

\section{Methods}{
  \describe{
    \item{get.alpha}{\code{signature(object = "lda")}:
      Get the scalar Dirichlet parameter alpha from the fitted model.}

    \item{get.beta}{\code{signature(object = "lda", log = "logical")}:
      Get the topics X terms beta matrix from the fitted model.  This
      matrix captures how closely each term is associated with each
      topic under the fitted model.  By default, this is a matrix of
      log-probabiaities.}

    \item{get.gamma}{\code{signature(object = "lda")}:
      Get the documents X topics gamma matrix from the fitted model.
      This matrix captures how closely each document is associated with
      each topic under the fitted model.}

    \item{num.terms}{\code{signature(object = "lda")}:
      Return the number of terms in the fitted model.}

    \item{num.topics}{\code{signature(object = "lda")}:
      Get the number of topics used in the fitted model.}

    \item{terms}{\code{signature(object = "lda")}:
      List the terms described by the fitted model.}

    \item{topics}{\code{signature(object = "lda", nterms="numeric")}:
      Returns an \code{nterms} X topics matrix listing the
      \code{nterms} words most closely associated with each topic
      under the fitted model.}
      
	 }
}
\references{D. Blei, A. Ng, and M. Jordan. 2003.  Latent Dirichlet
Allocation. \emph{Journal of Machine Learning Research}, 3:993--1022.}

\author{Anthony Fader, Gary King, Dan Pemstein, and Kevin Quinn}

\seealso{
  \code{\link{lda}}
  \code{\link{predict.lda}}
}

\keyword{classes}
