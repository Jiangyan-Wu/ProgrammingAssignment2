## Assume that the matrix supplied is always invertible. 
## Wriiting two R functions to create special objects that store 
## a square invertible matrix and cache its inverse 
## rather than compute the inverse repeatedly.

## makeCacheMatrix creates a special "matrix" object that can cache 
## the matrix and its inverse, and return a list of functions to 
## set/get the matrix and set/get the inverse of the matrix.

makeCacheMatrix <- function(x = matrix()) {
    im <- NULL
    set <- function(y) {
        x <<- y
        im <<- NULL
    }
    get <- function() x
    setinverse <- function(solve) im <<- solve
    getinverse <- function() im
    list(set = set, get = get,
         setinverse = setinverse,
         getinverse = getinverse)
}


## cacheSolve computes the inverse of the matrix returned 
## by makeCacheMatrix. If the inverse has already been calculated 
## (and the matrix has not changed), the cachesolve retrieves the inverse 
## from the cache

cacheSolve <- function(x, ...) {
    ## Return a matrix that is the inverse of 'x'
    im <- x$getinverse()
    if(!is.null(im)) {
        message("getting cached data")
        return(im)
    }
    data <- x$get()
    im <- solve(data, ...)
    x$setinverse(im)
    im
}