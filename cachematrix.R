
## This file provides functions to create a 
## special matrix which can cache its inverse
## 
## To test, you can do something like this:
##    cm <- makeCacheMatrix(test_mat)
##    cacheSolve(cm)

## This function creates a special "matrix" 
## object that can cache its inverse.

makeCacheMatrix <- function(x = matrix()) {
  
  # Initializing the inverse with NULL
  inv <- NULL
  
  # This function sets the internal value of x and inv
  set <- function(y){
    x <<- y
    inv <<- NULL
  }

  # Returns the value of 
  get <- function() x
  
  # This function sets the internal value 
  # of the inverse
  setinv <- function(new_inv) inv <<- new_inv 
  
  # This function gets the current internal 
  # value of the inverse
  getinv <- function() inv
  
  # Creates a list of functions and returns it 
  list( set = set, get=get, setinv = setinv, getinv = getinv )
  
}


## This function computes the inverse of the 
## special "matrix" returned by makeCacheMatrix 
## above. If the inverse has already been 
## calculated (and the matrix has not changed), 
## then the cachesolve should retrieve the 
## inverse from the cache.

cacheSolve <- function(x, ...) {
  ## Return a matrix that is the inverse of 'x'
  inv <- x$getinv()
  
  # Check if the value of the inverse 
  # has been calculated before
  if ( !is.null(inv) ){
    return (inv)
  }
  
  # If inverse has not been calculated before
  
  # Get the value of the matrix
  current_mat <- x$get()
  
  # Do the actual inversion
  inv <- solve(current_mat)
  
  # Set the internal value of the inv
  x$setinv(inv)
  
  # Return the inv
  inv
    
}
