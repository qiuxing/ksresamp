\name{Ndist}
\alias{Ndist}
\alias{Ndist.perm}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{An implementation of N-distance (N-statistic) functions.}
\description{
  An implementation of N-distance (N-statistic) functions.  A
permutation test based on the N-distance can be viewed as a
nonparametric and omnibus alternative to Mahalanobis distance.  This
test has been successfully used in detecting differentially expressed
genes, differentially associated genes, etc.
}
\usage{
Ndist(Xlist, Ylist, kernels=c("L1", "L2", "Linf"))

Ndist.perm(Xlist, Ylist, combs, kernels=c("L1", "L2", "Linf"))
}
%- maybe also 'usage' for other objects documented here.
\arguments{

  \item{Xlist, Ylist}{Two lists of multivariate samples to be compared.
    As an example, Xlist can be several FA maps sampled from the control
    group and Ylist can be FA maps sampled from the treatment group.
    arrays/vectors.  The elements in these lists must be either R
    vectors or arrays.
  }
  \item{kernels}{A list of kernels used in the N-distance.  Since
    the computational cost is very small, by default it computes them
    all.}

  \item{combs}{A list of permutations/combinations which can be
    generated by either \code{combn(N,Nx,function(x) c(x, setdiff(1:N,
    x)), simplify=FALSE)} or \code{foreach(icount(rand.comb)) \%do\%
    sample(N)}.  See \code{rep.test()} for more details.  }
}

\details{N-distance is numerically stable (no matrix inverse involved at
  all) and quite capable of detecting location shift (almost as powerful
  as t-test).  These functions are implemented in C for efficiency.
}

\value{A list of N-distances using different kernels (\code{Ndist()}) or
    an K by N dimensional matrix of N-distances (\code{Ndist.perm()}),
    where each row represents one permutation and each column a kernel.}
\author{Xing Qiu}
\examples{
## Generate some FA maps
N.x <- 32; N.y <- 32; N.z <- 3
for (i in 1:8) assign(paste("FA",i,sep=""), array(rnorm(N.x*N.y*N.z), c(N.x,N.y,N.z)))
grids <- list(2*(1:N.x)-1, 2*(1:N.y)-1, 2*(1:N.z)-1)
Xlist <- list(FA1, FA2, FA3, FA4); Ylist <- list(FA5, FA6, FA7, FA8)

Ndist(Xlist, Ylist) # a vector of length 3

## Now generate a list of 70 possible combinations.
combs <- combn(8, 4, function(x) c(x,setdiff(1:8,x)), simplify=FALSE)
Ndist.perm(Xlist, Ylist, combs) # a 70x3 matrix.

## You can also use a list of random permutations
combs2 <- foreach(icount(10)) \%do\% sample(8)
Ndist.perm(Xlist, Ylist, combs2) # a 10x3 matrix.

}                               % end examples.

% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{htest}
