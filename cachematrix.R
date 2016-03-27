## Put comments here that give an overall description of what your
## functions do

## Creates a special matrix that includes functions for 
## creating setting and getting the matrix and for getting
## and setting the matrix's inverse.

makeCacheMatrix <- function(x = matrix()) {
    i <- NULL
    set <- function(y) {
      x <<- y
      i <<- NULL
    }
    get <- function() x
    setinverse <- function(inverse) i <<- inverse
    getinverse <- function() i
    list(set = set, get = get, setinverse = setinverse,
         getinverse = getinverse)
}


## cacheSolve will return the inverse of a matrix.
## It will cache the inverse for future invocations.

cacheSolve <- function(x, ...) {
    ## Return a matrix that is the inverse of 'x'
    i <- x$getinverse()
    if(!is.null(i)) {
      message("getting cached data")
      return(i)
    }
    data <- x$get()
    i <- solve(data, ...)
    x$setinverse(i)
    i
}
