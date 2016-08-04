#' @export
caralloc = function(xmat,carwt,p,tol) {

  if (!is.matrix(xmat)) xmat = as.matrix(xmat)
  n = nrow(xmat)
  result = rbinom(n,1,0.5)
  if (n > 1) {
     for (j in 2:n) {

        # Find the absolute allocation imbalance for each covariate, with new observation allocated to each treatment

        matchx = apply(xmat[1:(j-1),,drop=FALSE],1,  function(x,xrow) { as.numeric(x == xrow) }, xmat[j,] )
        sumsofar = matchx %*% (2*result[1:(j-1)] - 1)
        imbalance1 = crossprod (abs(sumsofar + 1) , carwt)
        imbalance0 = crossprod (abs(sumsofar - 1) , carwt)
        if (imbalance1 < imbalance0 & imbalance0 >= tol) result[j] = rbinom(1,1,p)
        if (imbalance0 < imbalance1 & imbalance1 >= tol) result[j] = rbinom(1,1,1-p)
     }
  }
  result
}
