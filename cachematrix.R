## The following functions will complete two objective:
## 1. The first will create a list of functions that will 
##    be used to cache values based on a passed matrix "x"
## 2. The second will take the list of functions, and 
##    retrieve the cached values, or compute the inverse
##    of the matrix "x"
##

## This function will take a matrix "x" and create a list of
## functions to be called later, returning that list.

makeCacheMatrix <- function(x = matrix()) {
        m <- NULL
        set <- function(y) {
                x <<- y
                m <<- NULL
        }
        get <- function() x
        setinv <- function(inv) m <<- inv
        getinv <- function() m
        list(set = set, get = get,
             setinv = setinv,
             getinv = getinv)
}


## This matrix will take the list of functions from the 
## previous function and the passed matrix "x". It will 
## then compute the inverse of the matrix or return the 
## cached value

cacheSolve <- function(x, ...) {
        m <- x$getinv()
        if(!is.null(m)) {
                message("getting cached data")
                return(m)
        }
        data <- x$get()
        m <- solve(data,...)
        x$setinv(m)
        m
        ## Return a matrix that is the inverse of 'x'
}