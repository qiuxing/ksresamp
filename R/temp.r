## collection of older versions of functions.  To be deleted in a real package.

## ## Old internal function used by pre.post.test().  
## .norms.ks <- function(grids, DIFF, norm=c("L1","L2","Linf"), ...) {
##   fit.diff <- ksmooth.md(grids, DIFF, ...)
##   return(list("norms"=norms(fit.diff, norm=norm),
##               "fit.diff"=fit.diff,
##               "band.up"=max(fit.diff),
##               "band.down"=min(fit.diff)
##               ))
## }

## ## A local FDR (q-value) estimator based on tests and a quantile
## ## vector. Sort of poor man's BH procedure.  Slightly less accurate
## ## (who cares 0.001 of q-value?) but more efficient.
## .fdr.fun <- function(xvec, qvec, balanced=TRUE){
##   if (balanced){
##     ## p.orig <- sapply(abs(xvec), function(x) sum(abs(qvec)>=x)/length(qvec))
##     p.orig <- rev.rank(xvec, qvec)/length(qvec)
##   } else {
##     p.orig.lower <- rev.rank(xvec, qvec)/length(qvec)
##     p.orig <- pmin(1.0, 2*pmin(p.orig.lower, 1-p.orig.lower))
##   }
##   p.adjust(p.orig, "BH")
## }

## an older version of pre.post.test function
## pre.post.test <- function(grids, pre, post, bandwidth=5.0, perms=200, balanced=TRUE, ...){
##   Y <- post - pre                       #the difference map
##   fit.orig <- ksmooth.md(grids, Y, bandwidth=bandwidth, kernel="normal")
##   norms.orig <- norms(fit.orig)
##   ##usually Y is very large, we only record 0.1% quantiles for efficiency.
##   Nq <- min(1000, length(Y))
##   rr <- foreach (j=1:perms, .combine=.combfun) %dopar% {
##     Y.j <- flip(Y)
##     fit.j <- ksmooth.md(grids, Y.j, bandwidth=bandwidth, kernel="normal")
##     list("norms"=norms(fit.j), "band.up"=max(fit.j), "band.down"=min(fit.j),
##          "qvec"=quantile(fit.j, seq(0, 1, 1/Nq)))
##   }

##   qvec <- rr$qvec/perms
##   band.up <- rr$band.up; band.down <- rr$band.down

##   ## Inference
##   p.global <- foreach (n=names(norms.orig), .combine=c) %dopar% {
##     genboot.test(rr$norms[,n], norms.orig[n], ...)
##   }
##   fwer.adj.p <- array(.p.scb(fit.orig, band.up, band.down), dim(Y))
##   fdr.adj.p <- array(.fdr.fun(fit.orig, qvec), dim(Y))
##   return(list("p.global"=p.global,
##               "fwer.adj.p"=fwer.adj.p,
##               "fdr.adj.p"=fdr.adj.p,
##               "fit.orig"=fit.orig,
##               "band.up"=band.up,
##               "band.down"=band.down))
## }

#################### older stuff ####################


## pre.post.test.nofdr <- function(grids, pre, post, bandwidth=5.0, perms=200, ...){
##   Y <- post - pre                       #the difference map
##   fit.orig <- ksmooth.md(grids, Y, bandwidth=bandwidth, kernel="normal")
##   norms.orig <- norms(fit.orig)
##   rr <- foreach (j=1:perms, .combine=rbind) %dopar% {
##     Y.j <- flip(Y)
##     fit.j <- ksmooth.md(grids, Y.j, bandwidth=bandwidth, kernel="normal")
##     c(norms(fit.j), "band.up"=max(fit.j), "band.down"=min(fit.j))
##   }
##   band.up <- rr[,"band.up"]; band.down <- rr[,"band.down"];
##   ## Inference
##   p.global <- foreach (n=names(norms.orig), .combine=c) %dopar% {
##     genboot.test(rr[,n], norms.orig[n], ...)
##   }
##   fwerfun <- function(y) .p.scb(y, band.up, band.down)
##   fwer.adj.p <- array(sapply(fit.orig, fwerfun), dim(Y))
##   return(list("p.global"=p.global,
##               "fwer.adj.p"=fwer.adj.p,
##               "fit.orig"=fit.orig,
##               "band.up"=band.up,
##               "band.down"=band.down))
## }


## .Ndist.ks.nofit <- function(grids, Xlist, Ylist, norm=c("L1","L2","Linf"), ...) {
##   nx <- length(Xlist); ny <- length(Ylist); Ns <- dim(Alist[[1]])
##   X.ks <- foreach (x=Xlist) %do% ksmooth.md(grids, x, ...)
##   Y.ks <- foreach (y=Ylist) %do% ksmooth.md(grids, y, ...)
##   return(list("norms"=Ndist(X.ks, Y.ks, norm=norm)))
## }


## ## These functions are too slow to be usable for 3D data.

## ## This one is faster than Krig but not smooth
## Tps.md <- function(grids, yarray, theta=1.0, ...){
##   x <- expand.grid(grids); Y <- as.vector(yarray)
##   ## mod1 <- fastTps(x, Y, theta=theta, ...)
##   mod1 <- Tps(x, Y, theta=theta, ...)
##   return(array(predict(mod1), dim(yarray)))
## }

## ## this one is way too slow!
## Krig.md <- function(grids, yarray, lambda=1.0, ...){
##   x <- expand.grid(grids); Y <- as.vector(yarray)
##   ## mod1 <- mKrig(x, Y, lambda=lambda, ...)
##   mod1 <- Krig(x, Y, lambda=lambda, GCV=FALSE, ...)
##   return(array(predict(mod1), dim(yarray)))
## }



## rep.test <- function(grids, pre.list, post.list, bandwidth=5.0, perms=200, balanced=TRUE, norm=c("L1","L2","Linf"), kernel="normal", method=c("null", "normal"), MTP="BH", ...){
##   Nx <- length(pre.list); Ny <- length(post.list); Ns <- dim(pre.list[[1]])
##   X.ks <- foreach (x=pre.list) %dopar% {
##     ksmooth.md(grids, x, bandwidth=bandwidth, kernel=kernel)
##   }
##   Y.ks <- foreach (y=post.list) %dopar% {
##     ksmooth.md(grids, y, bandwidth=bandwidth, kernel=kernel)
##   }
##   fit.orig <- .list.array.mean(Y.ks) - .list.array.mean(X.ks)
##   norms.orig <- Ndist(X.ks, Y.ks, norm=norm)
##   rr <- foreach (j=1:perms, .combine=.combfun) %dopar% {
##     XY <- spatial.perm(c(pre.list, post.list))
##     X.ks <- foreach (x=XY[1:Nx]) %do% {
##       ksmooth.md(grids, x, bandwidth=bandwidth, kernel=kernel)
##     }
##     Y.ks <- foreach (y=XY[(Nx+1):(Nx+Ny)]) %do% {
##       ksmooth.md(grids, y, bandwidth=bandwidth, kernel=kernel)
##     }
##     fit.diff <- .list.array.mean(Y.ks) - .list.array.mean(X.ks)
##     list("norms"=Ndist(X.ks, Y.ks, norm=norm),
##          "band.up"=max(fit.diff),
##          "band.down"=min(fit.diff),
##          "pcounts"=rev.rank(fit.orig, fit.diff)
##          )
##   }
##   rawp <- rr$pcounts/(perms*prod(Ns))
##   band.up <- rr$band.up; band.down <- rr$band.down
##   ## Inference
##   p.global <- foreach (n=norm, .combine="cbind") %:% foreach (m=method, .combine="c") %dopar% {
##     genboot.test(rr$norms[,n], norms.orig[n], method=m, ...)
##   }; dimnames(p.global) <- list(method, norm)
##   fwer.adj.p <- .p.scb(fit.orig, band.up, band.down, balanced=balanced, method=method)
##   return(list("p.global"=p.global,
##               "fwer.adj.p"=fwer.adj.p,
##               "fdr.adj.p"=array(p.adjust(rawp, MTP), Ns),
##               "fit.orig"=fit.orig,
##               "band.up"=band.up,
##               "band.down"=band.down))
## }


## Ndist <- function(Xlist, Ylist, norm=c("L1", "L2", "Linf")){
##   ## Xlist, Ylist are lists of arrays/vectors.  Default Norm is set
##   ## to Linf. BG: between group dists; WGX, WGY are within group
##   ## dists.
##   Nx <- length(Xlist); Ny <- length(Ylist)
##   BG <- foreach(x=Xlist, .combine="+") %:% foreach(y=Ylist, .combine="+") %do% norms(x-y, norm=norm)
##   BG <- 2*BG/(Nx*Ny)
##   WGX <- foreach(i=2:Nx, .combine="+") %:% foreach(j=1:(i-1), .combine="+") %do% norms(Xlist[[i]]-Xlist[[j]], norm=norm);   WGX <- 2*WGX/(Nx^2)
##   WGY <- foreach(i=2:Ny, .combine="+") %:% foreach(j=1:(i-1), .combine="+") %do% norms(Ylist[[i]]-Ylist[[j]], norm=norm);   WGY <- 2*WGY/(Ny^2)
##   return(sqrt(BG-WGX-WGY))
## }

## .list.array.mean <- function(Alist){
##   ## when I have time, I need to rewrite the below (quite slow)
##   ## function in C.
##   Ns <- dim(Alist[[1]])
##   array(apply(matrix(unlist(Alist), nrow=prod(Ns)), 1, mean), Ns)
## }

