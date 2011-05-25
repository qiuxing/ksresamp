\name{ksmooth.md}
\alias{ksmooth.md}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{Multi-dimensional generalization of \code{ksmooth()} function.}
\description{
  Multi-dimensional generalization of \code{ksmooth()} function.
}
\usage{ksmooth.md(grids, yarray, bandwidth=5.0, ...)}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{grids}{A list of monotonically increasing coordinate grids.  For
    example, grids <- list(grid.x=seq(0, 100, 2), grid.y=exp(seq(1, 5, .2))).}
  \item{yarray}{An m-dim array defined on \code{grids} to be smoothed.}
  \item{bandwidth}{The bandwidth. The kernels are scaled so that their
    quartiles (viewed as probability densities) are at +/- '0.25*bandwidth'.}
  \item{...}{Other options passed to \code{ksmooth()} such as a choice
    of kernel.}
}
\details{
  This function is a multi-dimensional generalization of
  \code{ksmooth()}.  Care must be taken on the boundary points because
  currently there is no mechanism to take care of them automatically.  I may
  add some code to deal with the boundary points more gracefully in a
  future release.
}
\value{An array of the same dimension as \code{yarray}.}
\references{[kernel smoothing, multi-dim]}
\author{Xing Qiu}
\seealso{\code{\link{ksmooth}}}
\examples{
grid.x <- seq(1, 255, 2); grid.y <- seq(1, 255, 2); grid.z <- seq(1,5,2)
grids <- list(grid.x, grid.y, grid.z)
Y <- array(rnorm(128*128*3), c(128,128,3))
myfit <- ksmooth.md(grids, Y, bandwidth=5.0, kernel="normal")
}                               % end examples.

% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{smooth}
