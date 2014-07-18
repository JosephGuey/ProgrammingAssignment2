## Matrix inversion is usually a costly computation and there
## may be some benefit to caching the inverse of a matrix
## rather than compute it repeatedly. These two functions are
## using to calc and cache the inverse of a matrix.


## Create an object to access the matrix x and cache its inverse matrix
makeCacheMatrix <- function(x = matrix()) {
        inv <- NULL
        set <- function(y) {
                x <<- y
                inv <<- NULL
        }
        get <- function() {x}
        setinv <- function(y) {inv <<- y}
        getinv <- function() {inv}
        list(set = set, get = get, setinv = setinv, getinv = getinv)
}


## Get an inverse matrix from the object created by function makeCacheMatrix
## with an original matrix x. The matrix x must be square and invertible.
cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
        inv <- x$getinv()
        if (is.null(inv)) {
                inv <- solve(x$get())
                x$setinv(inv)
        } else {
                message("getting cached data")
        }
        inv
}