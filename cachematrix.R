## Caching the Inverse of a Matrix:
## There are two functions to create an object that 
## stores a matrix and caches the inverse.

## This function creates a "matrix" object that caches its inverse.

makeCacheMatrix <- function(x = matrix()) {
  inv <- NULL
  set <- function(z) {
    x <<- z
    inv <<- NULL
  }
  get <- function() x
  setInverse <- function(inverse) inv <<- inverse
  getInverse <- function() inv
  list(set = set,
       get = get,
       setInverse = setInverse,
       getInverse = getInverse)
}


## This function computes the inverse of the matrix given by makeCacheMatrix.
## It will give the inverse of the matrix if the inverse has already
## been calculated

cacheSolve <- function(x, ...) {
  ## Return the inverse matrix of x
  inv <- x$getInverse()
  if (!is.null(inv)) {
    message("getting cached data")
    return(inv)
  }
  mat <- x$get()
  inv <- solve(mat, ...)
  x$setInverse(inv)
  inv
}