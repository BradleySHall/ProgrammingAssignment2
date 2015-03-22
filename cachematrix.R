##  This file contains a pair of functions that make taking the inverse of a matrix
##  more computationally efficient.   

##  Specifically, if the inverse of the matrix has already been calculated, 
##  that inverse will be stored ("cached") with the matrix, 
##  so that it need not be re-calculated.
##
##  This caching (and retrieving) is implemented by 
##  turning the matrix into an "object" 
##  (that is, a data structure  that carries with it operations to perform on it)


## take a matrix, and return it as a data structure that is a list 
## which contains the matrix as well as a place for its inverse
## as well as four functions for operating on the matrix:
## setting the matrix,
## returning the matrix, 
## inverting the matrix, 
## and retrieving the cached inverted matrix
makeCacheMatrix <- function(x = matrix()) {
        m <- NULL
        set <- function(y) {
                x <<- y
                m <<- NULL
        }
        get <- function() x
        setSolve <- function(solve) m <<- solve
        getSolve <- function() m
        list(set = set, get = get,
             setSolve = setSolve,
             getSolve = getSolve)
}


## taking as its argument a previously created CacheMatrix, 
## either return the previously calculated inverse, or
## calculate, cache, and return the inverse.  Jolly good. 
cacheSolve <- function(x, ...) {
        m <- x$getSolve()
        if(!is.null(m)) {
                message("getting cached data")
                return(m)
        }
        data <- x$get()
        m <- solve(data, ...)
        x$setSolve(m)
        m
}
