## These functions were made to cache the inverse of a matrix and 
## to compute it.

## makeCacheMatrix is a function that creates a special "matrix",
## which can cache its inverse

makeCacheMatrix <- function(x = matrix()) {
    inverse <- NULL
    set <- function(y) {
        x <<- y
        inverse <<- NULL
    }
    get <- function() x
    setinverse <- function(solve) inverse <<- solve
    getinverse <- function() inverse
    list(set = set, get = get, setinverse = setinverse,
         getinverse = getinverse)
}

## cacheSolve is a function that computes the inverse of the "matrix"
## returned by makeCacheMatrix

cacheSolve <- function(x, ...) {
    inverse <- x$getinverse()
    if (!is.null(inverse)) {
        message("getting cached data")
        return(inverse)
    }
    data <- x$get()
    inverse <- solve(data)
    x$setinverse (inverse)
    inverse
}