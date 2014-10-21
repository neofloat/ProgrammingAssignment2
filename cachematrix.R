## This is solution to R Programming Assignment 2
##This function is used to compute the inverse of the special "matrix", and
##if the inverse has already been calculated (and the matrix has not changed),
##then the cachesolve retrieves the inverse from the cache.
##
## Usage example:
##
## m <- makeCacheMatrix(matrix(1:4, nrow=2))
## all(m$get() %*% cacheSolve(m) == diag(2)) # gives TRUE
## all(m$get() %*% cacheSolve(m) == diag(2)) # prints "getting cached data" and returns TRUE
##
## The function returns a list with four methods that let you:
## - set the matrix value
## - get the matrix value
## - set the inverse value of the matrix
## - get the inverse value of the matrix


makeCacheMatrix <- function(x = matrix()) {
        # The initial value of the inverse is set to NULL
        m <- NULL
        # First interface method: set a matrix value
        set <- function(y) {
                #  replace old x with new value y
                x <<- y
                # reset stored cache
                m <<- NULL
        }
        # Second interface method: get matrix value
        get <- function() x
        # Third interface method: set the inverse value of the matrix
        setinverse <- function(solve) m <<- solve
        # Fourth interface method: get the inverse value of the matrix
        getinverse <- function() m
        # list of all the functions withing this function is returned
        list(set = set, get = get,
             setinverse = setinverse,
             getinverse = getinverse)     
        
}


## This function is used to compute the inverse matrix.
## If the inverse of the same matrix has not been cached, it computes it and caches the value.
## Otherwise it reads and returns the cached inverse.
## The required parameter is the list with interface functions

cacheSolve <- function(x, ...) {
        # Read the inverse matrix through the interface function
        m <- x$getinverse()
        # If the inverse is cached, return its value
        if(!is.null(m)) {
                message("getting cached data")
                return(m)
        }
        data <- x$get()
        # Otherwise the inverse needs to be computed, cached and returned
        m <- solve(data, ...)
        # which we store in cachable matrix
        x$setinverse(m)
        # Return a matrix that is the inverse of 'x'
        m
}