## Test case:
##     source("cachematrix.R")
##     m <- matrix(c(4, 3, 3, 2), nrow=2, ncol=2, byrow=TRUE)
##     m
##     a <- makeCacheMatrix()
##     a$setmatrix(m)
##     cacheSolve(a)
##     cacheSolve(a)
## Inverse calculator: http://matrix.reshish.com/inverse.php


## Function creates matrix object that can cache 
##     the input matrix and its inverse.
makeCacheMatrix <- function(x = matrix()) {
    m <- NULL
    
    setmatrix <- function(y) {
        x <<- y
        m <<- NULL
    }
    getmatrix  <- function() x
    setinverse <- function(solve) m <<- solve
    getinverse <- function() m
    list(setmatrix=setmatrix, getmatrix=getmatrix, 
         setinverse=setinverse, getinverse=getinverse)
}


## Function computes the inverse of the matrix returned by makeCacheMatrix
##     function above.  If the inverse has already been calculated, the
##     inverse is simply retreived from the cache.
cacheSolve <- function(x=matrix(), ...) {
    m <- x$getinverse()
    if (!is.null(m)) {
        message("Getting cached data")
        return(m)
    }
    matrix <- x$getmatrix()
    m <- solve(matrix, ...)
    x$setinverse(m)
    return(m)
}
