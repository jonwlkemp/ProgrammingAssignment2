## These two functions allow you to compute the inverse of a square
## matrix and cache the result for subsequent calls

## This function creates a 'special' matrix function
## that allows you to get it's inverse. 

makeCacheMatrix <- function(x = matrix()) {

  i <- NULL
  set <- function(y) {
    x <<- y
    i <<- NULL 
  }
  get <- function() x
  setinv <- function(inv) i <<- inv
  getinv <- function() i
  list(set = set, get = get, setinv = setinv, getinv = getinv)
}


## This function checks to see whether the inverse
## has already been calculated, if it has, it returns from cache
## otherwise it calculates the inverse, sets it in the 'cache'
## and then returns it

cacheSolve <- function(x, ...) {
  i <- x$getinv()
  if (!is.null(i)) {
    message("Getting cached data")
    return(i)
  }
  data <- x$get()
  ##Could also use ginv from MASS package
  ##compute inverse of square matrix
  i <- solve(data, ...)
  ##Set inverse in cache
  x$setinv(i)
  ## Return a matrix that is the inverse of 'x'
  i
}
