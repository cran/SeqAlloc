#' @export
weight_x <-
function(xrow,xmat,carwt) {
    matchx = apply(xmat,1,function(x,xrow){as.numeric(x == xrow)},xrow)
    t(matchx) %*% carwt
}
