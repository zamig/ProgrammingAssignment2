## The makeCacheMatrix and cacheSolve function enable to calculate the inverse
## of a matrix while caching the result for future uses. 


## makeCacheMatrix is a special matrix object with a (cached) property of inverse and
## four methods of getting and setting the value of the matrix and getting and
## setting the inverse.

makeCacheMatrix <- function(x = matrix()) {

    inv <- NULL
    set <- function(m) {
        x <<- m
        inv <<- NULL
    }
    get <- function() x
    setinv <- function(mat_inv) inv <<- mat_inv
    getinv <- function() inv
    list(set = set, get = get, setinv = setinv, getinv = getinv)
}


## cacheSolve compute the inverse of the makeCacheMatrix object, using the
## caching ability of the special matrix object.

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
    
    inv = x$getinv()
    if (!is.null(inv)) {
        message("getting cached data")
        return(inv)
    }
    m <- x$get()
    inv <- solve(m, ...)
    x$setinv(inv)
    return(inv)
}
