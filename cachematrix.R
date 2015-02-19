## This code is written in fullfilment of Programming Assignment 2 of the course
## R Programming

## Creates a matrix object with cached inverse. Doesn't calculate the inverse by
## itself.
makeCacheMatrix <- function(x = matrix()) {
    cache.inv = NULL
    set <- function(m) {
        x <<- m
        cache.inv <<- NULL
    }
    get <- function() x
    set.inverse <- function(inv.m) cache.inv <<- inv.m
    get.inverse <- function() cache.inv
    list(set = set,
         get = get,
         set.inverse = set.inverse,
         get.inverse = get.inverse)
}


## Tries to fetch a pre-calculated inverse of a given matrix. If the inverse is 
## invalid or not yet computed the function computes and caches the inverse
## before returning it.
cacheSolve <- function(x, ...) {
    ## Return a matrix that is the inverse of 'x'
    cached <- x$get.inverse()
    if (is.null(cached)) {
        message("Calculating the inverse matrix.")
        m <- x$get()
        cached <- solve(m)
        x$set.inverse(cached)
    }
    else {
        message("Fetching cached inverse matrix.")
    }
    cached
}
