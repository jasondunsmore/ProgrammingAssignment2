## Calculate the inverse of a matrix, using the cached value if
## possible

## Create special "matrix" object that can cache its inverse

makeCacheMatrix <- function(x = matrix()) {
    i <- NULL
    set <- function(y) {
        x <<- y
        i <<- NULL
    }
    get <- function() x
    setsolve <- function(solve) i <<- solve
    getsolve <- function() i
    list(set = set, get = get,
         setsolve = setsolve,
         getsolve = getsolve)
}


## Compute the inverse of the special "matrix" object returned by
## makeCacheMatrix above. If the inverse has already been calculated
## and the matrix has not changed, then the cachesolve should retrieve
## the inverse from the cache.

cacheSolve <- function(x, ...) {
    ## Return a matrix that is the inverse of 'x'
    i <- x$getsolve()
    if(!is.null(i)) {
        message("getting cached data")
        return(i)
    }
    data <- x$get()
    i <- solve(data, ...)
    x$setsolve(i)
    i
}

# Example usage:
# 
# > m <- matrix(c(1,2,3,4), nrow=2, ncol=2)
# > mc <- makeCacheMatrix(m)
# > cacheSolve(mc)
#      [,1] [,2]
# [1,]   -2  1.5
# [2,]    1 -0.5
# > cacheSolve(mc)
# getting cached data
#      [,1] [,2]
# [1,]   -2  1.5
# [2,]    1 -0.5
