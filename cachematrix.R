## cacheSolve and makeCacheMatrix functions
## Description: caching of a mean vector
## source: cacheMatrix.R
## Example usage:
##      mtrx <- makeCacheMatrix(matrix(c(4,0,0,6),c(2,2)))
##      cacheSolve(mtrx) 
## sample output:
##           [,1]      [,2]
##      [1,] 0.25 0.0000000
##      [2,] 0.00 0.1666667

## makeCacheMatrix function creates a special "matrix" object that can cache its inverse

makeCacheMatrix <- function(x = matrix()) {
        mtrx <- NULL
        set <- function(y) {
                x <<- y
                mtrx <<- NULL
        }
        get <- function() x
        setinverse <- function(mean) mtrx <<- mean
        getinverse <- function() mtrx
        list(set = set, get = get,
             setinverse = setinverse,
             getinverse = getinverse)
}


## Compute inverse of the special Matrix returned by makeCacheMatrix

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
        mtrx <- x$getinverse()
        if(!is.null(mtrx)) {
                message("getting cached data")
                return(mtrx)
        }
        data <- x$get()
        mtrx <- solve(data, ...)
        x$setinverse(mtrx)
        mtrx
}
