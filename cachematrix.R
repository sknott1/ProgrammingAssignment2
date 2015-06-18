## This script contains two related functions makeCacheMatrix and cacheSolve.
## Together they give the inverse of a square, invertible matrix either
## calculated directly or, if the inverse has been previously calculated,
## retrieved from cache

## makeCacheMatrix creates a list containing 4 functions. These store and set
## the original matrix and, when available, the inverse.
## The input is a square, invertible matrix to be inverted.
makeCacheMatrix <- function(x = matrix()) {
        inv <- NULL     # sets inverse to NULL (i.e. not yet calculated)

# set: sets the matrix to be inverted and records no inverse has been calculated
        set <- function(y) {   
                x <<- y
                inv <<- NULL
        }
# get: prints the matrix to be inverted
        get <- function() x
# setinv: sets the inverse to be 'inverse' given through the function call
        setinv <- function(inverse) inv <<- inverse
# getinv: prints the inverse
        getinv <- function() inv
        list(set = set, get = get, setinv = setinv, getinv = getinv)
}


## cacheSolve returns the inverse of the matrix stored in makeCacheMatrix, either
## calculated from scratch or, if it already exists, retrieved from cache.
cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
        inv <- x$getinv()
# checks if inverse already exists, if so retrieves it
        if(!is.null(inv)) {       
                message("getting cached inverse")
                return(inv)            
        }
# inverse doesn't exist so retrieves matrix using get, inverts and stores inverse
# using setinv
        data <- x$get()   
        inv <- solve(data)
        x$setinv(inv)
        inv
}
