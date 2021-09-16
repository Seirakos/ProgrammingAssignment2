## Put comments here that give an overall description of what your
## functions do

## Write a short comment describing this function
##      this function creates a matrix object, in which it can cache its inverse
##      

makeCacheMatrix <- function(x = matrix()){
        inv <- NULL
        set <- function(y){
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

cacheSolve <- function(x, ...){
            inv <- x$getInverse()
            if (!is.null(inv)) {
              message("receiving cached data")
              return(inv)
            }
            mat <- x$get()
            inv <- mat(data, ...)
            x$SetInverse(inv)
            inv
}
