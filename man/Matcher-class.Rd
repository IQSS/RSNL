\name{Matcher-class}
\docType{class}
\alias{Matcher-class}
\alias{Matcher}
\alias{re.end}
\alias{re.end}
\alias{re.find}
\alias{re.groupCount}
\alias{re.group}
\alias{re.hasAnchoringBounds}
\alias{re.hasTransparentBounds}
\alias{re.hitEnd}
\alias{re.lookingAt}
\alias{re.matches}
\alias{re.regionEnd}
\alias{re.region}
\alias{re.regionStart}
\alias{re.replaceAll}
\alias{re.replaceFirst}
\alias{re.requireEnd}
\alias{re.reset}
\alias{re.start}
\alias{re.useAnchoringBounds}
\alias{re.usePattern}
\alias{re.useTransparentBounds}

\title{Class "Matcher"}
\description{An object that performs match operations on a
\code{character} string based on a \code{\linkS4class{Pattern}}.
These object wrap underlying Java Matcher objects.  See
http://java.sun.com/j2se/1.5.0/docs/api/java/util/regex/Matcher.html
for details.}

\section{Objects from the Class}{
Objects can be created by calls of the form \code{re.matcher(pattern,
input)} where \code{pattern} is an object of type
\code{\linkS4class{Pattern}} and \code{input} is a \code{character}
vector of length 1.}

\section{Methods}{
  \describe{
    \item{re.end}{\code{signature(object = "Matcher", group = "numeric")}:
      Returns the index+1 of the last subseqence captured by the given
      group during the last match.}
    
    \item{re.end}{\code{signature(object = "Matcher", group = "missing")}: 
      Returns the index+1 of the last character matched.}
    
    \item{re.find}{\code{signature(object = "Matcher", start = "numeric")}:
      Resets the matcher and then tries to find the next matching
      subsequence in the input, starting at index \code{start}.
      Returns a \code{logical} indicating whether or not a match was
      found.}
    
    \item{re.find}{\code{signature(object = "Matcher", start = "missing")}:
      Tries to find the next matching subsequence in the input.
      Returns a \code{logical} indicating whether or not a match was
      found.}
    
    \item{re.groupCount}{\code{signature(object = "Matcher")}:
      Returns the number of capturing groups specified in the
      matcher's pattern.}
    
    \item{re.group}{\code{signature(object = "Matcher", group = "numeric")}:
      Returns the input subsequence captured by the given group during
      the previous matche operation, if any.}
    
    \item{re.group}{\code{signature(object = "Matcher", group = "missing")}:
      Returns the most recently matched subsequence in the input, if
      any.}
    
    \item{re.hasAnchoringBounds}{\code{signature(object = "Matcher")}:
      Queries the anchoring of region bounds for this matcher.}
    
    \item{re.hasTransparentBounds}{\code{signature(object = "Matcher")}:
      Queries the transparency of region bounds for this matcher.}
    
    \item{re.hitEnd}{\code{signature(object = "Matcher")}:
      Returns true if the end of input was hit by the search engine in
      the last match operation performed by this matcher.}
    
    \item{re.lookingAt}{\code{signature(object = "Matcher")}:
      Attempts to match the input sequence, starting at the beginning
      of the region, against the pattern.}
    
    \item{re.matches}{\code{signature(object = "Matcher")}:
      Attempts to match the entire region against the pattern.  That
      is, it will return \code{TRUE} if, and only if, the entire input
      sequence matches the object's pattern.}
    
    \item{re.pattern}{\code{signature(object = "Matcher")}:
      Returns the pattern that is interpreted by this matcher.}
      
    \item{re.regionEnd}{\code{signature(object = "Matcher")}:
      Reports the end index (exclusive) of this matcher's region.}
    
    \item{re.region}{\code{signature(object = "Matcher", start = "numeric", end = "numeric")}:
      Sets the limits of this matcher's region, in indices into the
      input \code{character} string.}
    
    \item{re.regionStart}{\code{signature(object = "Matcher")}:
      Reports the start index of this matcher's region.}
    
    \item{re.replaceAll}{\code{signature(object = "Matcher", replacement = "character")}:
      Replaces every subsequence of the input sequence that matches
      the pattern with the given replacement string, returning a
      \code{character} string of the modified input.}
    
    \item{re.replaceFirst}{\code{signature(object = "Matcher", replacement = "character")}: 
      Replaces the first subsequence of the input sequence that
      matches the pattern with the given replacement string, returning
      a \code{character} string of the modified input.}
    
    \item{re.requireEnd}{\code{signature(object = "Matcher")}: 
      Returns true if more input could change a positive match into a
      negative one.}
    
    \item{re.reset}{\code{signature(object = "Matcher", input = "character")}:
      Resets this matcher with a new input string.}
    
    \item{re.reset}{\code{signature(object = "Matcher", input = "missing")}:
      Resets this matcher.}
    
    \item{re.start}{\code{signature(object = "Matcher", group = "numeric")}:
      Returns the start index of the subsequence captured by the given
      group during the previous match operation.}
    
    \item{re.start}{\code{signature(object = "Matcher", group = "missing")}:
      Returns the start index of the previous match.}
    
    \item{re.useAnchoringBounds}{\code{signature(object = "Matcher", b = "logical")}:
      Sets the anchoring of region bounds for this matcher.}
    
    \item{re.usePattern}{\code{signature(object = "Matcher", pattern = "Pattern")}:
      Changes the \code{\linkS4class{Pattern}} that this
      \code{Matcher} uses to find matches with.}
    
    \item{re.useTransparentBounds}{\code{signature(object = "Matcher", b = "logical")}:
      Sets the transparency of region bounds for this matcher.}
	 }
}

\references{Java 2 Platform, Standard Edition 5.0 API Specification, 
http://java.sun.com/j2se/1.5.0/docs/api/java/util/regex/Matcher.html}

\author{Anthony Fader, Gary King, Dan Pemstein, and Kevin Quinn}

\seealso{
  \code{\linkS4class{Matcher}}
}

\keyword{classes}
