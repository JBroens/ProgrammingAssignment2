## The function 'makeCacheMatrix' creates a special "matrix" object that can cache its inverse.
## This function 'cacheSolve' computes the inverse of the special "matrix" returned by the function
## 'makeCacheMatrix'. If the inverse has already been calculated (and the matrix has not changed), 
## then cacheSolve' retrieves the inverse from the cache.

## The function 'makeCacheMatrix' creates a special "matrix", which is really a list 
## containing a function to:
## -set the value of the vector
## -get the value of the vector
## -set the value of the mean
## -get the value of the mean

makeCacheMatrix <- function(x = matrix()) {
        
        m <- NULL
        set <- function(y) {
            x <<- y
            m <<- NULL
        }
        get <- function() x
        setinverse <- function(inverse) m <<- inverse
        getinverse <- function() m
        list(set = set, get = get,
             setinverse = setinverse,
             getinverse = getinverse)
    
    
}

## The function 'CacheSolve' computes the inverse of the special "matrix" created with the above function. 
## However, it first checks to see if the inverse has already been computed. If so, it gets the inverse
## from the cache and skips the computation. Otherwise, it computes the inverse of the matrix and sets 
## the inverse of the matrix in the cache via the setinverse function.

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
        m <- x$getinverse()
        if(!is.null(m)) {
            message("getting cached data")
            return(m)
        }
        data <- x$get()
        m <- solve(data, ...)
        x$setinverse(m)
        m
    
}
