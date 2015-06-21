## Put comments here that give an overall description of what your
## functions do

## Write a short comment describing this function
## This function create the set/get matrix functions along with its inverse

makeCacheMatrix <- function(x = matrix()) {
    # Assign NULL to matrix_inverse
    matrix_inverse = NULL
    
    # Set the matrix
    set = function(y) {
        # Assign a value "y" to "x" using <<- to set object in another environment
        x <<- y
        # Assignined NULL to matrix_inverse
        matrix_inverse <<- NULL
    }
    
    # Get the matrix
    get = function()
        x
    
    # Set the inverse of the matrix
    setinverse = function(inv)
        matrix_inverse <<- inv
    
    # Get the inverse of the matrix
    getinverse = function()
        matrix_inverse
    
    # Lists the functions
    list(
        set = set, get = get, setinverse = setinverse, getinverse = getinverse
    )
}


## Write a short comment describing this function
## Returns the inverse of the matrix 'x'

cacheSolve <- function(x, ...) {
    # Get the inverse of matrix 'x' using the function getinverse
    inverse = x$getinverse()
    
    # if the inverse exists
    if (!is.null(inverse)) {
        # retrieve the cached inverse matrix from the environment
        message("Returns the inverse of the matrix data from the cache")
        return(inverse)
    }
    
    # Call the get() to create the matrix data
    message("Gets matrix data")
    matrix_data = x$get()
    
    # Create the inverse of the data
    message("Sets the inverse of the matrix data")
    inverse = solve(matrix_data, ...)
    
    # Sets the matrix 'x' to cache of the environment
    message("Sets the inverse of the matrix data into cache")
    x$setinverse(inverse)
    
    # Retrieve the inverse matrix
    message("Returns the newly created inverse of the matrix data")
    # Prints Inverse
    inverse
}
