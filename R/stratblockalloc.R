#' @export
stratblockalloc <-
function(n,blksize) {
    
    result = NULL
    numcompleteblocks = n %/% blksize
    if (numcompleteblocks > 0) {
      block = c( rep(1:numcompleteblocks,each=blksize),rep(numcompleteblocks+1,n%%blksize) )
      for (i in 1:numcompleteblocks) {
      result = c(result,sample(c(rep(0,blksize/2),rep(1,blksize/2)),blksize,replace=F))
    }}
    if (numcompleteblocks ==0) {
       block = rep(1,n)
    }
    if (max(block) > numcompleteblocks) {
       result = c(result,sample(c(rep(0,blksize/2),rep(1,blksize/2)),n%%blksize,replace=F))
    }
    result
}
