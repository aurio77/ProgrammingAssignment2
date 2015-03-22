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
        
        set <- function(y) {
                x <<- y
                i <<- NULL
        }
        # "<<-" is used to assign a value to an object in an environment 
        # that is different from the current environment
        
        get <- function() x
        setinverse <- function(inverse) i <<- inverse
        getinverse <- function() i 
        
        list(set = set, 
             get = get, 
             setinverse = setinverse, 
             getinverse = getinverse)
        # Returns list of functions used as input for cacheSolve
        # setinverse: setting input inverse to i and returns inverse i
        # getinverse: returns cached inverse i
        
}

# FUNCTION 2
# This function computes the inverse of the special "matrix" 
# returned by makeCacheMatrix above. If the inverse has already
# been calculated (and the matrix has not changed), then the
# cachesolve should retrieve the inverse from the cache.

cacheSolve <- function(x, ...) {

        i <- x$getinverse()
        # Set i to get the inverse matrix in makeCacheMatrix 
        
        if (!is.null(i)){
                message("getting cached data")
                return(i)
        }
        # If inverse already calculated, get from cache, output message, skip
        # below steps and return the inverse i. Otherwise do the following.
        
        data <- x$get()
        # get the matrix and store as "data"
        i <- solve(data, ...)
        # get the inverse with the solve() function and store as i
        x$setinverse(i)
        # remember the inverse, i.e cache it via setinverse in makeCacheMatrix
        
        i
        # Return the inverse i
        
}
