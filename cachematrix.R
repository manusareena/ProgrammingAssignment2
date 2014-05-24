# Functions to create a cached matrix which caches its inverse computed and
# the function interface to compute the inverse of a matrix. The auxiliairy
# function makeCacheMatrix is used to store the cached value of inverse and
# return that as per request. Interface function cacheSolve retrieves the
# inverse from the cache and return. If the inverse is not already computed
# cacheSolve performs the necessary computation and caches the value before
# return the result.

# Function makeCacheMatrix: Function to create a cache matrix and cache
# the computed inverse of the matrix.
makeCacheMatrix <- function(mat = matrix())
{
    # Set the inverse of the matrix as NULL
    inverse <- NULL
    
    # Set matrix function to set the matrix and set the inverse to NULL
    setMatrix <- function(in_mat)
    {
        mat <<- in_mat
        inverse <<- NULL
    }
    
    # Return the matrix
    getMatrix <- function()
    {
        mat
    }
    
    # Set the inverse of the matrix to inv variable
    setInverse <- function(in_inv)
    {
        inverse <<- in_inv
    }
    
    # Return the inverse of the matrix
    getInverse <- function()
    {
        inverse
    }
    
    # Return the list of function
    list(setMatrix = setMatrix, getMatrix = getMatrix,
         setInverse = setInverse, getInverse = getInverse)
}

# Function cacheSolve: Returns the computed value of inverse. If the inverse
# is not already computed, it is computed and cached.
cacheSolve <- function(mat, ...)
{
    # Get the inverse of the function
    inv <- mat$getInverse()
    
    # If the inverse if already determined, return that
    if (!is.null(inv))
    {
        return(inv)
    }
    
    # If the inverse of the matrix is not already computed,
    # determine the inverse and update the cache with it.
    
    # Get the matrix
    m <- mat$getMatrix()
    
    # Compute the inverse of the matrix
    inv <- solve(m)
    
    # Set the inverse in the cache
    mat$setInverse(inv)
    
    # Return the inverse computed
    inv
}
