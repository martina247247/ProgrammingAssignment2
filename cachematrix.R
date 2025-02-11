# makeCacheMatrix: Creates a special "matrix" object that can cache its inverse
makeCacheMatrix <- function(x = matrix()) {
    # Initialize the cached inverse to NULL
    cached_inverse <- NULL
    
    # Define the set function
    set <- function(y) {
        x <<- y           # Assign the new matrix to x in the parent environment
        cached_inverse <<- NULL  # Reset the cached inverse since the matrix has changed
    }
    
    # Define the get function
    get <- function() x
    
    # Define the setinverse function
    setinverse <- function(inverse) cached_inverse <<- inverse
    
    # Define the getinverse function
    getinverse <- function() cached_inverse
    
    # Return a list of functions
    list(set = set, get = get, setinverse = setinverse, getinverse = getinverse)
}

# cacheSolve: Computes the inverse of the special "matrix" returned by makeCacheMatrix
cacheSolve <- function(x, ...) {
    # Retrieve the cached inverse
    inverse <- x$getinverse()
    
    # If the inverse is already cached, return it
    if (!is.null(inverse)) {
        message("Getting cached data")
        return(inverse)
    }
    
    # If the inverse is not cached, calculate it
    data <- x$get()
    inverse <- solve(data, ...)  # Compute the inverse using the solve function
    
    # Cache the calculated inverse
    x$setinverse(inverse)
    
    # Return the inverse
    inverse
}

# Example Usage
# Create a special matrix object
mat <- makeCacheMatrix(matrix(c(4, 3, 3, 2), 2, 2))

# Calculate the inverse (not cached yet)
print(cacheSolve(mat))  # Output: Inverse of the matrix

# Retrieve the inverse from the cache
print(cacheSolve(mat))  # Output: "Getting cached data" followed by the inverse

# Change the matrix
mat$set(matrix(c(1, 2, 3, 4), 2, 2))

# Calculate the inverse again (not cached yet)
print(cacheSolve(mat))  # Output: Inverse of the new matrix

# Retrieve the inverse from the cache
print(cacheSolve(mat))  # Output: "Getting cached data" followed by the inverse
