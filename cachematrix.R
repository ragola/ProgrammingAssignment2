## This pair of functions caches the inverse of a matrix

## The first function creates a matrix object that can 
## cache its inverse

makeCacheMatrix <- function(x = matrix()) {
    inv <- NULL
    set <- function(y) {
        x <<- y
        inv <<- NULL
    }
    get <- function() x
    setInverse <- function(inverse) inv <<- inverse
    getInverse <- function() inv
    list(set = set, get = get,
         setInverse = setInverse,
         getInverse = getInverse)
 }

## The second function computes the inverse of the 
## cached matrix above, if the inverse has already been
## calculated then the function retrieves the inverse
## from the cache

cacheSolve <- function(x, ...) {
  inv <- x$getInverse()
    if(!is.null(inv)) {
        message("getting cached data")
        return(inv)
    }
    data <- x$get()
    inv <- solve(data, ...)
    x$setInverse(inv)
    inv
}

## Return a matrix that is the inverse of 'x'
## Example below

x <- matrix(c(2, 4, 6, 8),
            nrow = 2,
            ncol = 2)
x
solve(x) 