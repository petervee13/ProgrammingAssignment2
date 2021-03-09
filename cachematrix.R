## A pair of functions that take as input an invertible matrix
## and cache the inverse of the matrix

## This function creates a special matrix object that can cache its inverse

makeCacheMatrix <- function(x = matrix()) {
        inverse <- NULL
        set <- function(y) {
                x <<- y
                inverse <<- NULL 
        }
        get <- function() x
        setinverse <- function(solve) inverse <<- solve 
        getinverse <- function() inverse
        list(set = set, get = get, 
             setinverse = setinverse, getinverse = getinverse)
}


## This function computes the inverse of the special matrix returned 
## by makeCacheMatrix

cacheSolve <- function(x, ...) {
        inverse <- x$getinverse()
        if(!is.null(inverse)) {
                message("getting cached data")
                return(inverse)
        }
        data <- x$get()
        inverse <- solve(data, ...)
        x$setinverse(inverse)
        inverse
}
