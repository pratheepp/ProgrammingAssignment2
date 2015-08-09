## Matrix inversion is usually a costly computation and there may be some 
## benefit to caching the inverse of a matrix rather than compute it repeatedly. 
## The following pair of functions create/change inverse of a matrix.


## This function creates a special "matrix" object that can cache its inverse.
## Assumption: The matrix supplied is always invertible.

makeCacheMatrix <- function(mat = matrix() ) {

    inv.matix <- NULL
    set <- function(mat) {
        this.mat <<- mat
        inv.matix <<- NULL
    }
    get <- function() mat
    set.inverse <- function(inv) inv.matix <<- inv
    get.inverse <- function() inv.matix
    list(set = set, get = get,
         set.inverse = set.inverse,
         get.inverse = get.inverse)
}



## This function computes the inverse of the special "matrix" returned by makeCacheMatrix. 
## If the inverse has already been calculated then the cachesolve retrieves the inverse from the cache.

cacheSolve <- function(cache.mat, ...) {
    cache <- cache.mat$get.inverse()
    if(!is.null(cache)) {
        message("getting cached matrix")
        return(cache)
    }
    data <- cache.mat$get()
    inverse <- solve(data, ...)
    cache.mat$set.inverse(inverse)
    inverse
}