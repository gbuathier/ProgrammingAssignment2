## caching time-computing computations

## makeCacheMatrix creates a matrix object that can cache the inverse

makeCacheMatrix <- function(M = matrix()) {
        invM <- NULL
        set <- function(P) {
                M <<- P
                invM <<- NULL
        }
        get <- function() M
        setinv <- function(inv) invM <<- inv
        getinv <- function() invM
        list(set = set, get = get,
             setinv = setinv,
             getinv = getinv)
}

## cacheSolve computes the inverse of the matrix returned by makeCacheMarix - if inverse
## has already been calculated, the function retrieves the inverse from the cache

cacheSolve <- function(M, ...) {
        invM <- M$getinv()
        if(!is.null(invM)) {
                message("getting cached data")
                return(invM)
        }
        data <- M$get()
        invM <- solve(data, ...)
        M$setinv(invM)
        invM
}