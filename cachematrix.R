## This project will compute the inverse of a matrix and save it in a cache. 
## If a cached version exists, it will retrieve the cached version rather than
## performing the computation a second time.
##
##
## makeCacheMatrix: This function creates a special "matrix" object that can 
## cache its inverse.

makeCacheMatrix <- function(x = matrix()) {
    ## using variable m because it was used in makeVector example
    m <- NULL  
    
    ## 1. set the value of the matrix
    set <- function(y) {
        x <<- y
        m <<- NULL
    }
    
    ## 2. set the value of the matrix
    get <- function() x
    
    ## 3. set the value of the inverse
    setinverse <- function(inverse) m <<- inverse
    
    ## 4.get the value of the inverse
    getinverse <- function() m
    list(set = set,
         get = get,
         setinverse = setinverse,
         getinverse = getinverse)
}


## cacheSolve: This function computes the inverse of the special "matrix" returned by makeCacheMatrix above. If the inverse has already been calculated (and the matrix has not changed), then the cachesolve should retrieve the inverse from the cache.

cacheSolve <- function(x, ...) {
    ## Return a matrix that is the inverse of 'x'
    ## Again, using m because this was the variable name in the example cachemean
    m <- x$getinverse() 
    if (!is.null(m)) { ## cached version already exists
        message("getting cached data")
        return(m)
    }
    data <- x$get()
    m <- solve(data, ...)
    x$setinverse(m)
    m
}
