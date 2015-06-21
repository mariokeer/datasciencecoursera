## makeCacheMatrix creates a special matrix object; cacheSolve returns the 
## inverse of the matrix from cache, or by calculating it.

## makeCacheMatrix creates a list of functions to set the matrix, get the matrix, set the inverse of the 
## matrix, and get the inverse of the matrix.

makeCacheMatrix <- function(x = matrix()) {
        inv <- NULL
        set <- function(y) { 
                x <<- y
                inv <<- NULL
        }
        get <- function() x
        setinverse <- function(inverse) inv <<- inverse
        getinverse <- function() inv
        list(set = set, get = get, setinverse = setinverse, getinverse = getinverse)
}


## cacheSolve returns the inverse of the matrix.
## If the matrix inverse has already been calculated, it will 
## find it in the cache and return it. If the matrix inverse has not yet been
## calculated, cacheSolve will calculate it, return it, and store it in cache.

cacheSolve <- function(x, ...) {
        inv <- x$getinverse()
        if (!is.null(inv)) {
                message("getting cached data...")
                return(inv)
        }
        data <- x$get()
        inv <- solve(data, ...)
        x$setinverse(inv)
        inv
}


