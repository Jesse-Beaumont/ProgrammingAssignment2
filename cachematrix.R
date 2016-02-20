## ---------------------------------------------------------
##               makeCacheMatrix
##
## This function encapsulates a matrix object
## with a set accessor methods so that it may be
## used with cacheSolve().
##
## ---------------------------------------------------------
makeCacheMatrix <- function(x = matrix()) {
    # initialize the scoped mean variable to NULL
    m <- NULL

    # set the matrix
    set <- function(y) {
        x <<- y
        m <<- NULL
    }

    # return the matrix
    get <- function() x

    # store the inversed matrix
    setmatrix <- function(solve) m <<- solve

    # return the inversed matrix
    getinversion <- function() m

    # return the list of function pointers
    list(set = set, get = get,
         setmatrix = setmatrix,
         getinversion = getinversion)
}

## ---------------------------------------------------------
##               cacheSolve
##
## This function perpetuates the inverse of a given matrix
## by returning an already processed result immediately
## or calculating the result for a new matrix and then
## storing that result for subsequent use.
##
##           ("Flyweight")
##
## ---------------------------------------------------------
cacheSolve <- function(x, ...) {
    ## Return a matrix that is the inverse of 'x'

    # flyweight: fetch inversion matrix
    m <- x$getinversion()

    # inversion has already been calculated, return cached value
    if(!is.null(m)) {
        message("getting cached data")
        return(m)
    }

    # fetch the vector
    data <- x$get()

    # calculate the mean
    m <- solve(data, ...)

    # cache the value
    x$setmatrix(m)
    m
}
