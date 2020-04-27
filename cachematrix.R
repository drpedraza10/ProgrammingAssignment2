## Use the cache to store values of the inverse
## matrix for later use with out having to recalculate it.

## Create an object with four functions to access the saved matrixes
## their inverses.

makeCacheMatrix <- function(x = matrix()) {
  i <- NULL
  set <- function(y) {
    x <<- y
    i <- NULL
  }
  get <- function() x
  setinverse <- function(inverse) i <<- inverse
  getinverse <- function() i
  list(set = set, get = get, 
       setinverse = setinverse, getinverse = getinverse)
}

## Gives access to the inverse of a matrix or assigns 
## the inverse to a matrix if the value is not found in cache

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
  i <- x$getinverse()
  if(!is.null(i)){
    message("getting cached data")
    return(i)
  }
  data <- x$get
  i <- solve(data, ...)
  x$setinverse(i)
  i
}
