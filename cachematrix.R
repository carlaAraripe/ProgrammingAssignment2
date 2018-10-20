## This functions cache the inverse of a matrix.

## Function makeCacheMatrix creates a special "matrix" object that can cache its inverse. This object has 4 methods that are 
## mutator and accessor methods for a matrix and its inverse. 

makeCacheMatrix <- function(x = matrix()) {
        invMtx <- NULL ## This object will receive the inverse matrix of the argument matrix x
        ## the method set() redefines the matrix in makeCacheMatrix
        set <- function(y) {
                ## assign y to the object x in the parent environment
                x <<- y
                ## assign NULL to the object invMtx in the parent environment 
                ## (since the matrix have changed, we will have to calculate the inverse matrix again)
                invMtx <<- NULL
        }
        get <- function() x
        setinverse <- function(inverse) invMtx <<- inverse
        getinverse <- function() invMtx
        list(set = set, get = get,
             setinverse = setinverse,
             getinverse = getinverse)
}


## Function cacheSolve computes the inverse of the special "matrix" returned by makeCacheMatrix above. 
## If the inverse has already been calculated (and the matrix has not changed), then the cachesolve should retrieve the inverse 
## from the cache.

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
        invMtx <- x$getinverse()
        if(!is.null(invMtx)) {
                message("getting inverse matrix from cached data")
                return(invMtx)
        }
        data <- x$get()
        invMtx <- solve(data, ...)
        x$setinverse(invMtx)
        invMtx
}
