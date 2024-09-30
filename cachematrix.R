
## Function to create a matrix that caches its inverse
makeCacheMatrix <- function(x = matrix()) {
    inv <- NULL
    
    # Function to set the matrix
    set <- function(y) {
        x <<- y
        inv <<- NULL
    }
    
    # Function to get the matrix
    get <- function() x
    
    # Function to set the inverse of the matrix
    setInverse <- function(inverse) inv <<- inverse
    
    # Function to get the inverse of the matrix
    getInverse <- function() inv
    
    # Return a list of all the functions
    list(set = set, get = get, setInverse = setInverse, getInverse = getInverse)
}

## Function to compute the inverse of the matrix
cacheSolve <- function(x, ...) {
    # Check if the inverse is already cached
    inv <- x$getInverse()
    if (!is.null(inv)) {
        message("getting cached data")
        return(inv)
    }
    
    # Get the matrix and calculate the inverse
    mat <- x$get()
    inv <- solve(mat, ...)
    
    # Cache the inverse
    x$setInverse(inv)
    
    # Return the inverse
    inv
}