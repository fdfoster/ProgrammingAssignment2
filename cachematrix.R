## Date: March 25, 2017
## R Programming Assignment 2

## makeCacheMatrix: This function creates a special matrix object
## that can cache its inverse

makeCacheMatrix <- function(x = matrix()) {
        invmatrix <- NULL        ##initializes x and invmatrix
        
        ## defines the "set" for the matrix
        set <- function(y) {
                x <<- y                         ## assigns value of y to x in parent environment
                invmatrix <<- NULL              ## assigns the value NULL to invmatrix in parent env.
        }
        
        ## defines the get for the matrix
        get <- function() x                     ## retrieves x from parent env.
        
        ## defines the set for the inverse matrix
        setinverse <- function(inverse) invmatrix <<- inverse
        
        ## defines the get for the inverse matrix
        getinverse <- function() invmatrix       ## retrieves invmatrix from parent  env.
        
        ## assigns each of the above functions as elements in a list, so
        ## you can extract each operator by name instead of [[ operator
        
        list(set = set, get = get, setinverse = setinverse, getinverse = getinverse)
}

## CacheSolve: This function computes the inverse of the special "matrix" 
## returned by makeCacheMatrix above. If the inverse has already been 
## calculated (and the matrix has not changed), then the cachesolve retrieves
## the inverse from the cache.

cacheSolve <- function(x, ...) {
        ## @x: output of makeCacheMatrix()
        ## return: inverse of the original matrix input to makeCacheMatrix()
        
        invmatrix <- x$getinverse()
        
        # if the inverse has already been calculated
        if (!is.null(invmatrix)){
                # get it from the cache and skips the computation. 
                message("getting cached data")
                return(invmatrix)
        }
        
        # otherwise, calculates the inverse 
        data <- x$get()
        invmatrix <- solve(data, ...)
        
        # sets the value of the inverse in the cache via the setinv function.
        x$setinverse(invmatrix)
        
        return(invmatrix)
}
