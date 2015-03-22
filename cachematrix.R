
# This function creates a special "matrix" object that can cache its inverse.                                                                                                                                                     changed), then the cachesolve should retrieve the inverse from the cache.

makeCacheMatrix <- function(x = matrix()) {
        
        inverse = NULL
        
        set = function(y) {
                x <<- y
                inverse <<- NULL
        }
        
        get = function() x
        setinverse = function(inverse) inv <<- inverse 
        getinverse = function() inverse
        
        list(set = set, get = get, setinverse = setinverse, getinverse = getinverse)

}


# This function computes the inverse of the special "matrix" 
# returned by makeCacheMatrix above. If the inverse has already
# been calculated (and the matrix has not changed), then the
# cachesolve should retrieve the inverse from the cache.

cacheSolve <- function(x, ...) {

        inverse <- x$getinverse()
        
        if (!is.null(inverse)){
                message("getting cached data")
                return(inverse)
        }
        
        data <- x$get()
        inverse <- solve(data, ...)
        x$setinverse(inverse)
        
        inverse
        
}
