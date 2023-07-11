\name{lda-methods}
\docType{methods}
\alias{lda}
\alias{lda,Corpus,ANY,ANY-method}
\alias{lda,DocumentTermMatrix,numeric,numeric-method}
\alias{lda,matrix,ANY,ANY-method}
\alias{lda,RSNLCorpus,ANY,ANY-method}
\alias{lda,TermDocumentMatrix,ANY,ANY-method}
\alias{lda,TokenizedCorpusView,ANY,ANY-method}

\title{lda}

\description{
  \code{lda} fits a latent Dirichlet allocation model to a corpus of
  text documents, using Blei et al's (2003) variational EM approach.
}


\usage{
  lda(object, alpha, k, var.max.iter=20, var.convergence=1e-6,
      em.max.iter=100, em.convergence=1e-4, estimate.alpha=TRUE)
}

\value{
  A fitted mode object of type \code{\linkS4class{lda}}.
}

\arguments{
\item{object}{An object representing a corpus of text.  A wide variety
of object types are permissible.}
\item{alpha}{The starting value for the model's Dirichlet parameter.}
\item{k}{The \emph{fixed} number of topics to model.}
\item{var.max.iter}{The number of coordinate ascent variational
inference iterations for a single document.  A value of -1 indicates
that the process should iterate until the convergence criterion is
met.}
\item{var.convergence}{The convergence criterion for variational
inference.  Stop if (a-b)/abs(a) is less than this value, where a
is the lower bound on the likelihood for the document from the
previous variational inference step and b is the current likelihood
lower bound.}
\item{em.max.iter}{The maximum number of EM iterations to perform.}
\item{em.convergence}{The convergence criterion for EM.  Stop if
(a-b)/abs(a) is less than this value, where a is the lower bound on
the likelihood for the corpus form the previous EM step and b is the
current likelihood lower bound.}
\item{estimate.alpha}{If set to \code{TRUE} the model estimates the
Dirichlet parameter along with the topic distributions, otherwise
alpha is left fixed at its starting value.}
}

\section{Methods}{
\describe{

\item{object = "DocumentTermMatrix", alpha = "numeric", k = "numeric"}{Apply lda to a \code{\link{DocumentTermMatrix}}.}

\item{object = "TermDocumentMatrix", alpha = "ANY", k = "ANY"}{Apply lda to a \code{\link{TermDocumentMatrix}}.}

\item{object = "matrix", alpha = "ANY", k = "ANY"}{Apply lda to a \code{matrix}.  The matrix is assumed to contain
document-term frequencies and should use terms as column names.}

\item{object = "Corpus", alpha = "ANY", k = "ANY"}{Apply lda to a \code{\linkS4class{Corpus}}.  The corpus will be tokenized
with the default \code{\linkS4class{Tokenizer}} before proceeding.}

\item{object = "RSNLCorpus", alpha = "ANY", k = "ANY"}{Apply lda to a \code{\linkS4class{Corpus}}.  The corpus will be tokenized
with the default \code{\linkS4class{Tokenizer}} before proceeding.}

\item{object = "TokenizedCorpusView", alpha = "ANY", k = "ANY"}{:  Apply lda to a \code{\link{TokenizedCorpusView}}.}
}}

\details{The \code{lda} method fits a latent Dirichlet allocation
(LDA) model to a corpus of text documents, returning a fitted model
object of type \code{\linkS4class{lda}}.  LDA is a generative
probabilistic model for discrete-data collections, most commonly
applied to text corpora.  LDA models each document in a collection as
a finite mixture over a latent topic set, while each underlying topic
is modeled as an infinite mixture over topic probablities.  The method
operates on a variety of objects that may represent a document corpus.
This method uses Blei et al's (2003) variational EM algorithm.}

\references{D. Blei, A. Ng, and M. Jordan. 2003.  Latent Dirichlet
Allocation. \emph{Journal of Machine Learning Research}, 3:993--1022.}

\author{Anthony Fader, Gary King, Dan Pemstein, and Kevin Quinn}

\seealso{
  \code{\linkS4class{lda}}
  \code{\link{predict.lda}}
}

\keyword{methods}
