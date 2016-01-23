## Put comments here that give an overall description of what your
## functions do

##Creating a matrix, and a fuction for caching the inverse matrix to original one. 

## Write a short comment describing this function


## the makeCacheMatrix creates a matrix with given functions:
## cache - holder of inversed matrix. By defaul is null.
## set - set a new matrix, in other words resubmit a new matrix to value x.
## get - returns the matrix x
## getInverse - returns the inversed matrix to x, if any already has been computed.


makeCacheMatrix <- function(x = matrix()) {
                cache <- NULL
                
                set <- function(newMatrix) {
                        x <<- newMatrix
                        cache <<- NULL
                }
                get <- function() {x}
                cacheInverse <- function(inverse) {
                        cache <<- inverse }
                getInverse <- function() {cache} 
                list( set = set, get = get, cacheInverse = cacheInverse, getInverse = getInverse)

}


## Write a short comment describing this function
## Checks of cache contains the inversed matrix
        ## if yes, returns the value from cache
        ## otherwise, computes the inverse matrix and saved the value in cache, and returns it. 

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
        inverse <- x$getInverse()
        if(!is.null(inverse)) {
                message("getting the caching data")
                return(inverse)
        }
        matrix <- x$get()
        inverse <- solve(matrix)
        x$cacheInverse(inverse)
        inverse
}