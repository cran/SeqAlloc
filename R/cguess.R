#' @export
cguess <-
function(alloc) {

  guess = rep(NA,length(alloc))
  cprob = c(0.5,cumsum(alloc)/(1:length(alloc)))
  cprob = cprob[1:length(alloc)]
  guess[cprob > 0.5] = 0
  guess[cprob < 0.5] = 1
  numhalf = sum(is.na(guess))
  if (numhalf > 0) guess[is.na(guess)] = 0.5 # rbinom(numhalf,1,0.5)

  1 - abs(guess - alloc) # = 1 if correct guess, 0 otherwise

}
