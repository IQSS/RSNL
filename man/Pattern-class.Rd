\name{Pattern-class}
\docType{class}
\alias{Pattern-class}
\alias{Pattern-methods}
\alias{Pattern}
\alias{re.findall}
\alias{re.flags}
\alias{re.matcher}
\alias{re.pattern}
\alias{re.split}
\alias{re.compile}
\alias{re.sub}
\alias{re.matchesPart}
\alias{show,Pattern-method}

\title{Class "Pattern"}
\description{A "Pattern" represent a compiled regular expression.
These objects wrap underlying Java Pattern objects and perform
computations in Java under the hood.  See
http://java.sun.com/j2se/1.5.0/docs/api/java/util/regex/Pattern.html
for details on the underlying implementation.}

\section{Objects from the Class}{
Object can be created with calls of the form \code{re.compile(pattern,
unix.lines=FALSE, case.insensitive=FALSE, comments=FALSE,
multiline=FALSE, literal=FALSE, dotall=FALSE, unicode.case=TRUE,
canon.eq=TRUE)} where \code{pattern} is a string containing a regular
expression.  The other parameters set various flags.}

\section{Methods}{
  \describe{
    \item{re.findall}{\code{signature(pattern = "Pattern",
                                      input = "character")}: 
      Returns a vector of all non-overlapping matches of \code{pattern}
      in the given \code{input}, which should be a \code{character}
      vector of length 1.}
    
    \item{re.flags}{\code{signature(pattern = "Pattern")}:
      Returns a vector of the flags that are set for \code{pattern}.}
    
    \item{re.matcher}{\code{signature(pattern = "Pattern",
                                      input = "character")}:
      Returns a \code{\linkS4class{Matcher}} that applies 
      \code{pattern} to \code{input}.}
    
    \item{re.pattern}{\code{signature(object = "Pattern")}:
      Returns a \code{character} representation of \code{object}.}
    
    \item{re.split}{\code{signature(object = "Pattern",
                                    input = "character")}:
      Splits \code{input} by \code{pattern}, returning a
      \code{character} vector.  The \code{input} must consist of a
      single string element.}

    \item{re.matchesPart}{\code{signature(object = "Pattern",
                                          input = "character")}:
      Takes a \code{character} vector of input strings and attempts to
      match \code{object} against each string, returning a vector of
      \code{logical} values indicating if \code{object} matched any
      part of each input string.}

    \item{re.sub}{\code{signature(object = "Pattern", input = "character",
                                  repl = "character")}:
      Takes a \code{character} vector of input strings and replaces
      every match of \code{object} in each string with a string from
      \code{repl}.  The \code{repl} vector will be recycled as needed
      and one element from \code{repl} serves as the replacement
      string for each string in the \code{input} vector.}
      
	 }
}
\references{Java 2 Platform, Standard Edition 5.0 API Specification, 
http://java.sun.com/j2se/1.5.0/docs/api/java/util/regex/Pattern.html}

\author{Anthony Fader, Gary King, Dan Pemstein, and Kevin Quinn}

\seealso{
  \code{\linkS4class{Matcher}}
}

\keyword{classes}
