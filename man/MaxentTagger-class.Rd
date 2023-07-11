\name{MaxentTagger-class}
\docType{class}
\alias{MaxentTagger-class}
\alias{language,MaxentTagger-method}
\alias{show,MaxentTagger-method}
\alias{tag,character,MaxentTagger,missing,missing-method}
\alias{tag,character,MaxentTagger,PTBTokenizer,missing-method}

\title{Class "MaxentTagger"}

\description{Assigns part of speech tags to tokens using maximum
entropy models.}

\section{Objects from the Class}{ Objects can be created by calls of
the form \code{MaxentTagger(language="english")} where \code{language}
can be one of \code{c("english", "german", "arabic", "chinese")}.
These objects provide an interface to the Stanford Log-linear
Part-Of-Speech Tagger, developed by the Stanford Natural Language
Processing Group.  Note that \code{MaxentTagger} engines are
static; once you load a particular tagging engine within a single R
session all \code{MaxentTagger}s that you construct from that point on
will use the same engine (so, in other words, you cannot currently tag
English and German words in the same R session).}

\section{Extends}{
Class \code{"\linkS4class{Tagger}"}, directly.
}

\section{Methods}{
  \describe{
    \item{language}{\code{signature(object = "MaxentTagger")}:
      Returns the language engine used by the tagger.}
	 }
}

\references{
\item{}{Kristina Toutanova and Christopher D. Manning. 2000.
Enriching the Knowledge Sources Used in a Maximum Entropy
Part-of-Speech Tagger. In Proceedings of the Joint SIGDAT Conference
on Empirical Methods in Natural Language Processing and Very Large
Corpora (EMNLP/VLC-2000), pp. 63-70.}
\item{}{Kristina Toutanova, Dan Klein, Christopher Manning, and Yoram
Singer. 2003. Feature-Rich Part-of-Speech Tagging with a Cyclic
Dependency Network. In Proceedings of HLT-NAACL 2003, pp. 252-259.}
}

\author{Anthony Fader, Gary King, Dan Pemstein, and Kevin Quinn}

\seealso{
  \code{\link{tag}}
  \code{\linkS4class{Tagger}}
  \code{\link{MaxentTagger}}
}

\keyword{classes}
