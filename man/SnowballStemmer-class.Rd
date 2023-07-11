\name{SnowballStemmer-class}
\docType{class}
\alias{SnowballStemmer-class}
\alias{language,SnowballStemmer-method}
\alias{stem,character,SnowballStemmer,missing,missing-method}

\title{Class "SnowballStemmer"}

\description{
  A \code{\linkS4class{Stemmer}} wrapping Rstem's Snowball stemmer.
  Implementations are available in multiple languages.  Use Rstem's
  \code{getStemLanguage()} function to see a list of available
  languages.
}

\section{Objects from the Class}{
Objects can be created by calls of the form
\code{SnowballStemmer(lang)}.
}

\section{Extends}{
Class \code{"\linkS4class{Stemmer}"}, directly.
Class \code{"\linkS4class{TokenTransform}"}, by class "Stemmer",
distance 2.
}

\section{Methods}{
  \describe{
    \item{language}{\code{signature(object = "SnowballStemmer")}:
      Returns the language used by the stemmer. }

    \item{stem}{\code{signature(object = "character", stemmer = "SnowballStemmer", tokenizer = "missing", index = "missing")}:
      Applies the stemmer to a \code{character} vector of tokens and
      returns a \code{character} vector of stemmed tokens.  Note that
      this method will apply the default tokenizer to generate a token
      vector if \code{object} happens to be a
      \code{\linkS4class{TextDocument}}. }
	 }
}

\details{Like all \code{\linkS4class{Stemmer}} objects,
\code{SnowballStemmer} objects may be applied using the
\code{\link{stem}} method via a version of the method specialized on
\code{\linkS4class{Stemmer}}.  Nonetheless, like all
\code{\linkS4class{Stemmer}} sub-types, they technically implement the
\code{\linkS4class{TokenTransform}} interface.}

\author{Anthony Fader, Gary King, Dan Pemstein, and Kevin Quinn}

\seealso{
  \code{\link{SnowballStemmer}}
  \code{\link{stem}}
  \code{\linkS4class{Stemmer}}
}

\keyword{classes}
