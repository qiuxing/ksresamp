\name{rep.test}
\alias{rep.test}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{Detecting abnormal region from several repeated pre and post images}
\description{
  1. kernel smoothing; 2. compute the mean diff image; 3. compute the
  original N-stat; 4. use permutation to generate null distributions;
  5. global hypothesis testing based on N-stats; 6. local hypothesis
  testing based on mean diff image, both FWER and FDR adjusted p-values.
}
\usage{
rep.test(grids, pre.list, post.list, bandwidth=5.0, perms=200,
balanced=TRUE, norm=c("L1","L2","Linf"), kernel="normal",
method=c("null", "normal"), MTP="BH", ...)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{grids}{A list of monotonically increasing coordinate grids.  For
    example, grids <- list(grid.x=seq(0, 100, 2), grid.y=exp(seq(1, 5,
    .2))).}
  \item{pre.list}{A list of m-dim array defined on \code{grids}, the
    "pre" scans.}
  \item{post.list}{A list of m-dim array defined on \code{grids}, the
    "post" scans. pre/post lists may have different lengths.}
  \item{bandwidth}{The bandwidth. The kernels are scaled so that their
    quartiles (viewed as probability densities) are at +/- '0.25*bandwidth'.}
  \item{perms}{Number of permutations to run.  For usual nonparametric
    p-value estimation I recommend using 1000 permutations; for smoothed
    (generalized bootstrap) p-value estimation I recommend using 200
    permutations.  Default to 200.}
  \item{balanced}{A logical variable used by the built-in FWER
    controlling MTP.  Balanced: I assume the difference is symmetrically
    distributed around zero, so \code{abs(diff)} is used in the
    hypothesis testing.  Unbalanced: conduct two separate one-sided test
    for \code{diff} to obtain \code{p.up}, \code{p.down}; the two sided
    p-value is then defined as \code{min(1.0, 2*min(p.up, p.down))}.
    This p-value is conservative and asymptotically exact.  Default: balanced=TRUE.}
  \item{norm}{A list of functional norms as summary statistics.  Since
    the computational cost is very small, by default it computes them
    all.}
  \item{kernel}{Either "box" or "normal".  See help of \code{ksmooth()}
    for more details.  Default to a normal kernel.}
  \item{method}{Smoothing method used by the generalized bootstrap
    p-value estimation.  Valid methods: gld (genral lambda
    distribution, depends on package \code{gld}), sn (skew-normal,
    depends on package \code{sn}), normal, and null (the usual
    nonparametric estimate). Default value: \code{c("null", "normal")}.}
  \item{MTP}{Specify the multiple testing procedure used to compute
    \code{fdr.adj.p}. Default to \code{BH}.  See help of
    \code{p.adjust()} for more options.}
  \item{...}{other optional parameters passed to \code{starship()} or
    \code{sn.em()}, which are the parameter estimating function for
    general lambda distribution and skew-normal distribution, resp.}
}
\details{
  Under construction.
}
\value{A list with components
  \item{p.global}{A list of global p-values computed by N-statistic with
    several norms.}
  \item{fwer.adj.p}{An array of per-voxel p-values adjusted for
    familywise error rate.}
  \item{fdr.adj.p}{An array of per-voxel p-values adjusted for
    false discovery rate.}
  \item{fit.diff}{An array of fitted (smoothed) difference map.}
  \item{band.up}{A vector of upper bounds of fitted difference map
    for each permutation.}
  \item{band.down}{A vector of lower bounds of fittted difference map
    for each permutation.}
}
\references{[some new papers]}
\author{Xing Qiu}
\seealso{\code{\link{genboot.test}}, \code{\link{ksmooth.md}}}
\examples{
## Generate a small data
Nx <- 12; Ny <- 12; Nz <- 3
for (i in 1:5) assign(paste("FA",i,sep=""), array(rnorm(Nx*Ny*Nz), c(Nx,Ny,Nz)))
grid.x <- (1:Nx)*2-1; grid.y <- (1:Ny)*2-1; grid.z <- (1:Nz)*2-1; 
grids <- list(grid.x, grid.y, grid.z)
Xlist <- list(FA1, FA2, FA3); Ylist <- list(FA4, FA5)

## Will take a little while to run
\dontrun{r2 <- rep.test(grids, Xlist, Ylist, method="normal")}
}                               % end examples.

% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{smooth}
\keyword{htest}% __ONLY ONE__ keyword per line
