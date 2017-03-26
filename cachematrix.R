## Caching the Inverse of a Matrix:

makeCacheMatrix <- function(x = matrix()) {
    inv <- NULL # Define the function to set the value of the matrix and clear the old inverse
    set <- function(y) {
        x <<- y  # Set the value
        inv <<- NULL # Clear the cache
    }
    get <- function() x 
    setInverse <- function(inverse) inv <<- inverse # Define function to set the inverse.
    getInverse <- function() inv # Define function to get the inverse
    list(set = set, # Return a list 
         get = get,
         setInverse = setInverse,
         getInverse = getInverse)
}


## Return the inverse of the matrix

cacheSolve <- function(x, ...) {
    ## Return a matrix that is the inverse of x
    inv <- x$getInverse()
    if (!is.null(inv)) {
        message("getting cached data")
        return(inv)
    }
    mat <- x$get() # get the value of the matrix
    inv <- solve(mat, ...) # calcuate the inverse 
    x$setInverse(inv) # cache the results
    inv # return the inverse 
}

