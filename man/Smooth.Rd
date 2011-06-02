\name{Smooth}
\alias{Smooth}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{Multi-dimensional smoothing function.}
\description{
  Multi-dimensional smoothing function.
}
\usage{Smooth(grids, yarray, bandwidth=5.0, sm.method="ksmooth", ...)}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{grids}{A list of monotonically increasing coordinate grids.  For
    example, \code{grids <- list(grid.x=seq(0, 100, 2),
    grid.y=exp(seq(1, 5, .2))).}}
  \item{yarray}{An m-dim array defined on \code{grids} to be smoothed.}
  \item{bandwidth}{The bandwidth. The kernels are scaled so that their
    quartiles (viewed as probability densities) are at +/-
    '0.25*bandwidth'.}
  \item{sm.method}{Smoothing method. Valid choices: 1. \code{ksmooth} (kernel
    smoothing with a gauss kernel, the default choice), 2. \code{tps}
    (thin plate spline).}
  \item{...}{Other options passed to different smoothers such as a choice
    of kernel (\code{kernel=box}) or various options of function
    \code{Tps()} implemented in \pkg{fields}.}
}
\details{
  This function is a wrapper of different smoothing methods.  The
  default method, "ksmooth", is a multi-dimensional generalization of
  \code{ksmooth()}.  Care must be taken on the boundary points because
  currently there is no mechanism to take care of them automatically.  I may
  add some code to deal with the boundary points more gracefully in a
  future release.  Another method, "tps", is the thin plate spline
  smoother, depends on package \pkg{fields}.  This function is too slow
  for very large scale data, it is included primarily as an example.
  Feel free to implement your own choice of smoother(s) and replace this
  function by your custom function.
}
\value{An array of the same dimension as \code{yarray}.}
\references{[kernel smoothing, multi-dim]}
\author{Xing Qiu}
\seealso{\code{\link{ksmooth}}}
\examples{
grid.x <- seq(1, 255, 2); grid.y <- seq(1, 255, 2); grid.z <- seq(1,5,2)
grids <- list(grid.x, grid.y, grid.z)
Y <- array(rnorm(128*128*3), c(128,128,3))
myfit1 <- Smooth(grids, Y, bandwidth=5.0, kernel="normal")
myfit2 <- Smooth(grids, Y, bandwidth=5.0, kernel="box")
## The following command depends on package fields.
\dontrun{myfit3 <- Smooth(grids, Y, bandwidth=5.0, sm.method="tps")}
}                               % end examples.

% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{smooth}
