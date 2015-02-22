## Matrix inversion is usually a costly computation and there may be 
## some benefit to caching the inverse of a matrix rather than compute it repeatedly

## This function creates a special "matrix" object that can cache its inverse.
## This function uses "solve" function available in R to compute matrix inverse
## It is assumed the input matrix is always a square invertible matrix and no 
## checking ids performed on the input.

makeCacheMatrix <- function(x = matrix()) {
        
        inverse <- NULL
        set <- function(y) {
                x <<- y
                inverse <<- NULL
                
        }
        get <- function() x
        
        setinverse <- function(solve) inverse <<- solve
        
        getinverse <- function() inverse
        
        ## list of available functions
        list(set = set, get = get,
             setinverse = setinverse,
             getinverse = getinverse)

}

## This function computes the inverse of the special "matrix" returned by makeCacheMatrix
## above. If the inverse has already been calculated (and the matrix has not changed), 
## then the cachesolve should retrieve the inverse from the cache.

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
        
        ## get cached mean if available
        inverse <- x$getinverse()
        
        ## check for cached mean 
        if(!is.null(inverse)) {
                message("getting cached data")
                return(inverse)
        }
        
        z <- x$get()
        
        inverse<- solve(z, ...)
        
        x$setinverse(inverse)
        inverse 
}
