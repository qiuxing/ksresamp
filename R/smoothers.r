#### Collections of all smoothing related functions.

## This is a wrapper around the ksmooth() function.
.ksmooth.md <- function(grids, yarray, bandwidth=5.0, kernel="normal", ...){
  ## multi-dim ksmooth wrapper.  grids: a list of (marginal)
  ## coordinates. yarray: an m-dim array of values to be smoothed.
  ## dim(yarray) must be sapply(grids, length) otherwise this function
  ## won't work.
  ## Still needs some work for one dim array.
  Y <- as.array(yarray)
  if (any(dim(Y) != sapply(grids, length))){
    stop("Dimensions of coordinates (grids) and values (yarray) do not match.")
  } else K <- length(dim(Y))
  y.k <- Y
  for (k in 1:K){
    y.k <- aperm(apply(y.k, seq(K)[-k], function(y) ksmooth(grids[[k]], y, n.points=length(grids[[k]]), bandwidth=bandwidth, kernel=kernel, ...)$y), append(2:K, 1, k-1))
  }
  return(y.k)
}

## This is the thin plate spline smoother, depends on package
## "fields".  This function is too slow for large scale data, it is
## included as an example.
.tps.md <- function(grids, yarray, bandwidth=5.0, ...){
  x <- expand.grid(grids); Y <- as.vector(yarray)
  ## mod1 <- fastTps(x, Y, theta=theta, ...)
  mod1 <- fastTps(x, Y, theta=bandwidth, ...)
  return(array(predict(mod1), dim(yarray)))
}

## wrapper function for different spatial smoothers
Smooth <- function(grids, yarray, bandwidth=5.0, sm.method="ksmooth", ...) {
  if (!is.function(sm.method)){
    ys <- switch(sm.method,
                 "ksmooth"=.ksmooth.md(grids, yarray, bandwidth, ...),
                 "tps"=.tps.md(grids, yarray, bandwidth, ...),
                 stop("Valid smoothing methods: 1. ksmooth (kernel smoothing, the default choice), 2. tps (thin plate spline)."))
  } else {                 #sm.method() is a custom smoothing function
    ys <- sm.method(grids, yarray, bandwidth, ...)
  }
    return(ys)
  }

