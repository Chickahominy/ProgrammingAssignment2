## The following functions are used to find the inverse of a given matrix and cache
## the result. Caching the result has the potential to save time as it eliminates the 
## need for repeated computations if the matrix in question (and thus its inverse) do
## not change.

## The makeCacheMatrix function creates a special "matrix" object that can cache its inverse.

makeCacheMatrix <- function(x = matrix()) {
  m <- NULL
  set <- function(newMatrix) {
    x <<- newMatrix
    m <<- NULL
  }
  get <- function() {
    x
  }
  setInverse <- function(input) {
    m <<- input
  }
  getInverse <- function() {
    m
  }
  list(set = set, get = get,
       setInverse = setInverse,
       getInverse = getInverse)
}


## The "cacheSolve" function computes the inverse of the special "matrix" 
## returned by "makeCacheMatrix" above. If the inverse has already been 
## calculated (and the matrix has not changed), then `cacheSolve` should 
## retrieve the inverse from the cache.

cacheSolve <- function(x, ...) {
  m <- x$getInverse()
  if(!is.null(m)) {
    message("getting cached data")
    return(m)
  }
  data <- x$get()
  m <- solve(data, ...)
  x$setInverse(m)
  m
}
