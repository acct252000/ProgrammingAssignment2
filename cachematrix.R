## As Matrix inversion is a costly function, these functions are used to cache
## the inverse of a matrix rather than computing it repeatedly.

## This function creates a matrix object that can cache its inverse

makeCacheMatrix <- function(x = matrix()) {
    m <- NULL
    set <- function(y) {
        x <<- y
        m <<- NULL
    }
    get <- function()
        x
    setinverse <- function(inverse)
        m <<- inverse
    getinverse <- function()
        m
    list(
        set = set,
        get = get,
        setinverse = setinverse,
        getinverse = getinverse
    )
    
}


## This function either computes the inverse of the matrix object above or
## returns the previously computed inverse from the cache.

cacheSolve <- function(x, ...) {
    ## Return a matrix that is the inverse of 'x'
    m <- x$getinverse()
    if (!is.null(m)) {
        message("getting cached data")
        return(m)
    }
    data <- x$get()
    m <- solve(data)
    x$setinverse(m)
    m
}
