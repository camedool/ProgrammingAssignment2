##Creating a function for matrix creation, and a fuction for caching the inverse matrix to original one. 
## Consist of 2 functins: 1. makeCacheMatrix(x); 2. cacheSolve(x)
## ----------------------------------------------------------------
        
        ## makeCacheMatrix(x) creates a matrix with given functions
        
                ## cache - holder of inversed matrix. By defaul is null.
                ## set - set a new matrix, in other words resubmit a new matrix to value x.
                ## get - returns the matrix x.
                ## getInverse - returns the inversed matrix to x, if any already has been computed.


makeCacheMatrix <- function(x = matrix()) {
                cache <- NULL
                
                set <- function(newMatrix) {
                        x <<- newMatrix
                        cache <<- NULL #resetting cache, as we have a new matrix now and new inverse should be computed.
                }
                
                get <- function() {x}
                
                cacheInverse <- function(inverse) {
                        cache <<- inverse }
                
                getInverse <- function() {cache} 
                list( set = set, get = get, cacheInverse = cacheInverse, getInverse = getInverse)

}

##-------------------------------------------------------------------

        ## cahceSolve checks of cache contains the inversed matrix
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