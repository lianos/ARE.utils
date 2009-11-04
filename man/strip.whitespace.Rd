\name{strip.whitespace}
\alias{stirp.whitespace}
\title{Remove Surrounding flanking whitespace in strings}
\description{
This function removes the leading/trailing whitespace from the
elements in a character vector (and optionally removes whitespace
in the entire string)
}

\usage{
strip.whitespace(from, internal=FALSE)
}

\arguments{
  \item{from}{A vector of characters}
  \item{internal}{
    Logical indicating whether whitespace \textbf{inside} string should be
    removed.
  }
}


\details{
There really isn't much more to it than that.
}

\value{
A vector of equal length as \code{from} with the flanking whitespace
removed from its elements. If \code{internal = TRUE}, the whitespace
\textbf{inside} the strings are removed, as well.
}

\author{Steve Lianoglou}

% \seealso{
%   \code{\link{strip.slashes}}
% }
\examples{
}

% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{utilities}
