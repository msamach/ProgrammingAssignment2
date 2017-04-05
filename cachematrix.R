## These functions work to cache the inverse of a matrix
## to potentially save computation time

## This function creates a pseudo "matrix" object
## and can cache its inverse

makeCacheMatrix <- function(x = matrix()) {
  I <- NULL
  set <- function(g) {
    x <<- g
    I <<- NULL
  }
  get <- function() x
  setInv <- function(Inv) I <<- Inv
  getInv <- function() I
  
  list(set = set, get = get, setInv = setInv, getInv = getInv)
}


## This function returns the inverse of the psuedo matrix object seen above
## If inverse has already been calculated and matrix hasn't changed, 
## inverse is retrieved by cache.

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
  I <- x$getInv()
  if (!is.null(I)) {
      message ("getting cached data")
      return(I)
  }
  
  data <- x$get()
  I <- solve(data, ...)
  x$setInv(I)
  I
}
