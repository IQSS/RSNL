\name{Stemmer-class}
\docType{class}
\alias{Stemmer-class}
\alias{Stemmer}
\alias{stem,ANY,Stemmer,ANY,ANY-method}
\alias{transformTokens,character,Stemmer,missing,missing-method}


\title{Class "Stemmer"}

\description{
  An abstract class representing an object that takes a sequence of
  tokens and shortens them to stems by removing common endings.
}

\section{Extends}{
Class \code{"\linkS4class{TokenTransform}"}, directly.
}  

\section{Objects from the Class}{A virtual Class: No objects may be created from it.}

\section{Methods}{
  \describe{
    \item{stem}{\code{signature(object = "ANY", stemmer = "Stemmer")}:
      Apply the given stemmer to \code{object}.  This method invokes
      \code{\link{transformTokens}} under the hood.}
    
    \item{transformTokens}{\code{signature(object = "character",
                                           transform = "Stemmer",
                                           tokenizer = "missing",
                                           index = "missing")}:
      Stemmers are sub-type of \code{\linkS4class{TokenTransform}} and
      therefore implement said interface.  Nonetheless, to simplify
      stemmer implementation, we require only that \code{Stemmer}
      sub-types implement the \code{stem} base case (\code{stem}
      specialized for \code{character} objects).  This method
      specialization redirects base-case \code{tokenTransform} calls
      to the appropriate \code{stem} specialization.}
	 }
}

\details{
  See \code{\link{stem}} for more details on applying \code{Stemmer}
  objects.
}

\author{Anthony Fader, Gary King, Dan Pemstein, and Kevin Quinn}

\seealso{
  \code{\link{stem}}
  \code{\link{transformTokens}}
  \code{\linkS4class{SnowballStemmer}}
  \code{\linkS4class{TokenTransform}}
}

\keyword{classes}
