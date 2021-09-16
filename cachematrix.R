## Put comments here that give an overall description of what your
## functions do

## Write a short comment describing this function
##      this function creates a matrix object, in which it can cache its inverse
##      

makeCacheMatrix <- function(x = matrix()){
        ## sets inverse as null
        inv <- NULL
        set <- function(y){ ##this sets the value of the cached matrix according to x
                  x <<- y
                  inv <<- NULL
        }
        get <- function() {x}
        setInverse <- function(inverse) {inv <<- inverse}
        getInverse <- function() {inv}
        list(set = set, get = get, setInverse = setInverse, getInverse = getInverse)
}


## Write a short comment describing this function
## this function returns a matrix that is the inverse of 'x'
cacheSolve <- function(x, ...) {
        inv <- x$getInverse()
        ## checks if there is a cached inverse of the x.
        if (!is.null(inv)) {
        message("receiving cached data")
        return(inv)
        }
        mat <- x$get()
        inv <- solve(mat, ...)
        x$setInverse(inv)
        inv
}
