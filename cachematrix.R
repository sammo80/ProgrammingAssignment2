## These functions work together to create an inverse of a square matrix
## and cache the result. If the function cachesolve is called, it first 
## searches for the inverse in cache and returns the cached result (if it 
## exists and there have been no changes to the original matrix).

## The first function, makeCacheMatrix, creates a special "matrix" that
## can cache its inverse.

makeCacheMatrix <- function(x = matrix()) {
    inv <- NULL
    set <- function(y) {
        x <<- y
        inv <<- NULL
    }
    get <- function() x
    setinverse <- function(solve) inv <<- solve
    getinverse <- function() inv
    list(set = set, get = get,
         setinverse = setinverse,
         getinverse = getinverse)
    
}


## The second function, cacheSolve, computes the inverse of the special 
## matrix returned by makeCacheMatrix. If the inverse has already been 
## calculated (and the matrix has not changed), then cacheSolve 
## retrieves the inverse from the cache.

cacheSolve <- function(x, ...) {
        
    inv <- x$getinverse()
    if(!is.null(inv)) {
        message("getting cached data")
        return(inv)
    }
    data <- x$get()
    inv <- solve(data, ...)
    x$setinverse(inv)
    inv
    
}
