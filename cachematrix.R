## These functions compute the inverse of a matrix, and save it to 
## a cache to be retrieved, preventing repeated computation of the 
## inverse.

## The function makeCacheMatrix creates the object "matrix" which 
## can cache the inverse of the matrix contained inside the object.

makeCacheMatrix <- function(x = matrix()) {
        "matrix" <- NULL
        set <- function(y) {
                x <<- y
                "matrix" <<- NULL
        }
        get <- function() x
        setinv <- function(solve) "matrix" <<- solve
        getinv <- function() "matrix"
        list(set = set, get = get,
             setinv = setinv,
             getinv = getinv)
}

## The function cacheSolve computes the inverse of the "matrix" 
## returned by makeCacheMatrix. If the inverse has already been 
## calculated, cacheSolve will retrieve the inverse from the cache.

cacheSolve <- function(x, ...) {
        "matrix" <- x$getinv()
        if(!is.null("matrix")) {
                message("getting cached data")
                return("matrix")
        }
        data <- x$get()
        "matrix" <- solve(data, ...)
        x$setinv("matrix")
        "matrix"
}
