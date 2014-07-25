## The following two functions enable the caching and retrieval of matrix inverses.
## The original matrix must have the same number of rows and columns, and consist of real
## numbers.

## makeCacheMatrix consists of a series of functions that store both the matrix
## and the inverse of the matrix in cache and that allow for retrieval of either.

makeCacheMatrix <- function(x = matrix()) {
  inverse <- NULL
  setMatrix <- function(y) {
    x <<- y
    inverse <<- NULL
  }
  setInverse <- function(y) {
    inverse <<- solve(y)
  }
  getMatrix <- function() x
  getInverse <- function() inverse
  list(setMatrix = setMatrix, getMatrix = getMatrix,
       setInverse = setInverse, getInverse = getInverse)
}


## cacheSolve takes an object created by makeCacheMatrix and retrieves the inverse
## of the matrix (as provided in makeCachMatrix) if it has previously been stored. 
## Otherwise, it creates the inverse of the matrix and stores it for later retrieval.

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
  
  i <- x$getInverse()
  
  if (!is.null(i)) {
    message("getting cached data")
    return(i)
  }
  m <- x$getMatrix()
  i <- x$setInverse(m)
  i
}
