\name{spatial.perm}
\alias{spatial.perm}
\alias{flip}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{An implementation of N-distance (N-statistic) functions.}
\description{
  An implementation of N-distance (N-statistic) functions.  A
permutation test based on the N-distance can be viewed as a
nonparametric and omnibus alternative to Mahalanobis distance.  This
test has been successfully used in detecting differentially expressed
genes, differentially associated genes, etc.  In this package these
functions are written in pure R because computing N-statistics takes
only a very small fraction of total computing time.
}
\usage{
spatial.perm(XYlist)

flip(diff.array)
}
%- maybe also 'usage' for other objects documented here.
\arguments{

  \item{XYlist}{Lists of multivariate samples to be spatially permuted.
    As an example, it can be a list of FA maps sampled from both
    groups. The elements in this list must be either R vectors or
    arrays. }

  \item{diff.array}{An array/vector of pre/post differences.}
  }
}
\details{
  ....
}
\value{A list of N-distances using different kernels (\code{Ndist()}) or
    an K by N dimensional matrix of N-distances (\code{Ndist.perm()}),
    where each row represents one permutation and each column a kernel.}
\author{Xing Qiu}
\examples{
## Generate some FA maps
N.x <- 64; N.y <- 64; N.z <- 3
for (i in 1:8) assign(paste("FA",i,sep=""), array(rnorm(N.x*N.y*N.z), c(N.x,N.y,N.z)))
grids <- list(2*(1:N.x)-1, 2*(1:N.y)-1, 2*(1:N.z)-1)
Xlist <- list(FA1, FA2, FA3, FA4); Ylist <- list(FA5, FA6, FA7, FA8)

Ndist(Xlist, Ylist) # a vector of length 3

## Now generate a list of all 70 possible combinations.
combs <- combn(8, 4, function(x) c(x,setdiff(1:8,x)), simplify=FALSE)
Ndist.perm(Xlist, Ylist, combs) # a 70x3 matrix.

## You can also use a list of random permutations
combs2 <- foreach(icount(10)) %do% sample(8)
Ndist.perm(Xlist, Ylist, combs2) # a 10x3 matrix.

}                               % end examples.

% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{htest}
