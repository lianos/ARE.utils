\name{packages.dump}
\alias{packages.load.R}
\alias{packages.load.bioc}

\title{Package Upgrade Management}

\description{
These functions help manage your libraries as you upgrade to a new version
of R. The code in these functions was extracted from:

http://onertipaday.blogspot.com/2008/10/r-upgrade-on-mac-os-x-1055-leopard.html
}

\usage{
# Before upgrading R versions, run this from your current R
packages.dump('~/Rpackages.rda')

# Upgrade R to the latest and greatest.
# To reinstall the same packages you had from your previous R install:
packages.load.R('~/Rpackages.rda')

# Once this finishes, anything that was not installed will just
# emit warnings. Assuming that the rest of your libs are from
# bioconductor, you can try install again via:
packages.load.bioc

# Now get back to work
}

\arguments{
  \item{dump.file}{The file path to where you want to save/load your list of installed packages}
}


\details{
There really isn't much more to it than that.
}

\value{
A new version of R that has the same packages that were installed in your previous version.
}

\author{Steve Lianoglou}

\seealso{
  http://onertipaday.blogspot.com/2008/10/r-upgrade-on-mac-os-x-1055-leopard.html
}

% \examples{
% }

% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{utilities}
