## Abstract
## This file includes two functions for the optimization of the calculations of
## an inverse of a matrix.

## What makeCacheMatrix does?
## This function creates a special "matrix" object that can cache its inverse.
## It's prepared a list that can receive an invertible matrix and store it's 
## inverse. For every invertible matrix, after the first run of cacheSolve 
## (explained below) this list will be updated with the data of the matrix 
## presented: the matrix itself and the inverse.

makeCacheMatrix <- function(x = numeric()) {
        
        m <- NULL
        
        set <- function(y) {
                x <<- y
                m <<- NULL
        }
        
        get <- function() x
        
        setinverse <- function(inverse) m <<- inverse
        
        getinverse <- function() m
        
        list(set = set, get = get,
             setinverse = setinverse,
             getinverse = getinverse)
}


## What cacheSolve does?
## This function computes the inverse of the special "matrix" returned by 
## makeCacheMatrix above. If the inverse has already been calculated (and the 
## matrix has not changed), then the cachesolve should retrieve the inverse from 
## the cache.

cacheSolve <- function(x, ...) {
        m <- x$getinverse()
        
        ## when it is not the first time that the matrix is presented to cacheSove
        ## it will consult the inverse matrix stored in the list (a.k.a. special 
        ## "matrix")
        if(!is.null(m)) {
                message("getting cached data")
                return(m)
        }
        
        ## if it is the first time that the matrix is presented, it's effectively
        ## computed the inverse matrix and it's stored in cache for future usage
        data <- x$get()
        m <- solve(data, ...)
        x$setinverse(m)
        m
}