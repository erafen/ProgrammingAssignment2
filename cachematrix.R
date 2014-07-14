## -----------------------------------------------------------------------
## This file contains 2 functions for caching matrices:
##
## makeCacheMatrix: creates the needed functions to set and get the value 
##                  of a square matrix and its inverse, providing a 
##                  mechanism for caching these values.
##
## cacheSolve.....: returns the inverse of a square matrix. Once the inverse
##                  created, it is stored using the set function defined in
##                  makeCacheMatrix so it can be retrieved at any time with 
##                  no need to recompute it.
##
## To create a cached matrix, use the following code:
##
##               myCacheMatrix <- makeCacheMatrix(myMatrix)
##
## where myMatrix is a previously defined matrix, e.g:
##     mymatrix <- matrix(data = c(1,2,3,4), nrow=2, ncol=2))
##
## To get the inverse of a cached matrix, use:
##
##               myInvertedMatrix <- cacheSolve(myCacheMatrix)
##
## where myCacheMatrix is the value returned by makeCacheMatrix(myMatrix)
## -----------------------------------------------------------------------


## makeCacheMatrix function
## args...: a matrix (optional, defaults to an empty one)
## returns: a list of setter and getter functions of the cached matrix

makeCacheMatrix <- function(x = matrix()) {
    
    # m will contain the inverse of the matrix
    m <- NULL
    
    # Define setter and getter of the matrix
    set <- function(y) {
        x <<- y
        m <<- NULL
    }
    get <- function() x

    # Define setter and getter of the inverse of the matrix
    setinverse <- function(solve) m <<- solve
    getinverse <- function() m
    
    # Returns a list with the definition of setters and getters
    list(set = set, 
         get = get, 
         setinverse = setinverse,
         getinverse = getinverse)
}


## cacheSolve function
## args...: 
##       a cached matrix (mandatory)
##       extra args to be passed to the solve function (optional)
##
## returns: the inverse of the matrix

cacheSolve <- function(x, ...) {
    
    # Getting the inverse of the matrix
    m <- x$getinverse()
    if(!is.null(m)) {
        message("getting cached data")
        return(m)
    }
    
    # No cached data available
    # Computing the inverse
    data <- x$get()
    m <- solve(data, ...)
    x$setinverse(m)
    m
}
