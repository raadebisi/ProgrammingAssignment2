## Put comments here that give an overall description of what your
## functions do
# These functions work together to accept a square matrix and calculate
# the inverse of the matrix. It also stores these values for future retrieval.

## Write a short comment describing this function
# The function makeChacheMatrix accept a square matrix and used the matrix in
# four other functions: set, get, setinv and getinv.
# set function is used to set the given matrix as the matrix in the main function
# get function can be used to retrieve the current matrix
# setinv function can be used to set the values of inverse of matrix
#        this is necessary if one need to override the current values inverse matrix
# getinv function can be used to retrieve the current values of inverse matrix.

makeCacheMatrix <- function(x = matrix()) {
    inv <- NULL
    set <- function(y) {
        x <<- y
        inv <<- NULL
    }
    get <- function() x
    setinv <- function(solve) inv <<- solve
    getinv <- function() inv
    list(set = set, get = get, setinv = setinv, getinv = getinv)
}


## Write a short comment describing this function
# The function cacheSolve checks if the inverse value has already been calculated by 
# using !is.null function. If that is true it returns the stored value in the 
# function getinv() as the inverse value. 
# Otherwise, it will solve for the inverse and set the answer as the new inverse value.

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
    inv <- x$getinv()
    if(!is.null(inv)) {
        message("getting cached data")
        return(inv)
    }
    data <- x$get()
    inv <- solve(data, ...)
    x$setinv(inv)
   inv
}
