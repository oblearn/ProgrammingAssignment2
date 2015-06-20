# Returns a list of functions that wrap a matrix.
# The functions allow for the following:
#   Setting the value of the matrix
#   Getting the value of the matrix
#   Setting the value of the inverse of the matrix
#   Getting the value of the inverse of the matrix
makeCacheMatrix <- function(wrappedMatrix = matrix()) {
    # Variable to hold the computed inverse
    cachedInverse <- NULL

    # set updates the wrapped matrix
    set <- function(matrixToSet) {
        # update the wrapped matrix value
        wrappedMatrix <<- matrixToSet
        # reset the cache since a new matrix has been set
        cachedInverse <<- NULL
    }

    # get() returns the wrapped matrix
    get <- function() wrappedMatrix

    # setinverse updates the cached inverse value
    setinverse <- function(computedInverse) cachedInverse <<- computedInverse

    # getinverse returns the cached inverse value
    getinverse <- function() cachedInverse

    # return list of functions
    list(set = set, get = get, setinverse = setinverse, getinverse = getinverse)
}

# Takes a 'cacheMatrix' and returns the inverse
# The inverse is taken from the cache if already computed or
# computed and cached first if not
cacheSolve <- function(cachedMatrix, ...) {
    inverse <- cachedMatrix$getinverse()
    if(!is.null(inverse)) {
        message("getting cached inverse");
        return(inverse)
    }

    data <- cachedMatrix$get()
    inverse <- solve(data,...)
    cachedMatrix$setinverse(inverse)
    inverse
}
