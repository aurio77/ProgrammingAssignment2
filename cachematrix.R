# 2 functions to cache the inverse 

# FUNCTION 1
# This function creates a special "matrix" object that can cache its inverse.
# Similar to the makeVector function, the makeCacheMatrix returns a list of 
# functions used as input to cacheSolve:
# {set the matrix, get the matrix, set the inverse, get the inverse}


makeCacheMatrix <- function(x = matrix()) {
        # Initialise function & specify input x to be a matrix 
        
        i = NULL
        # Intialise local i to be NULL
        
        set = function(y) {
                x <<- y
                i <<- NULL
        }
        # <<- used to assign a value to an object in an environment 
        # that is different from the current environment
        
        get = function() x
        setinverse = function(inverse) i <<- inverse 
        getinverse = function() i
        
        list(set = set, 
             get = get, 
             setinverse = setinverse, 
             getinverse = getinverse)
        # Returns list of functions used as input for cacheSolve
        
}

# FUNCTION 2
# This function computes the inverse of the special "matrix" 
# returned by makeCacheMatrix above. If the inverse has already
# been calculated (and the matrix has not changed), then the
# cachesolve should retrieve the inverse from the cache.

cacheSolve <- function(x, ...) {

        i <- x$getinverse()
        # Set inverse matrix
        
        if (!is.null(i)){
                message("getting cached data")
                return(i)
        }
        # If inverse already calculated, get from cache, output message, skip
        # below steps and return the inverse
        
        # Otherwise get the inverse with the solve() function and store as below
        
        data <- x$get()
        i <- solve(data, ...)
        x$setinverse(i)
        
        i
        # Return the inverse
        
}
