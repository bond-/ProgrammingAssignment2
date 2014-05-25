## This function creates a special "matrix" object that can cache its inverse
makeCacheMatrix <- function(x = matrix()) {
  ## Setting the inverse value to NULL
  i <- NULL
  
  ## Function to change the matrix in use
  set <- function(y) {
    x <<- y
    i <<- NULL
  }
  
  ## Function to get the matrix being used
  get <- function() x
  
  ## Sets the inverse of the marix
  setInverse <- function(inverse) i <<- inverse
  
  ## Gets the inverse of the matrix
  getInverse <- function() i
  
  ## Return a list of methods that can perform action on the matrix 'x'
  list(set = set, get = get,
       setInverse = setInverse,
       getInverse = getInverse)
}


## This function computes the inverse of the special "matrix" returned by makeCacheMatrix above.
## If the inverse has already been calculated (and the matrix has not changed), then cacheSolve
## should retrieve the inverse from the cache.
##
## Here x should be the return value of makeCacheMatrix function
cacheSolve <- function(x, ...) {
  ## Fetch the inverse of matrix stored in the variable 'x'
  i <- x$getInverse()
  
  ## If the inverse of matrix stored in 'x' has been computed, then print a log message and return
  ## the cached value
  if(!is.null(i)) {
    message("getting cached data")
    return(i)
  }
  
  ## Get the matrix in use in the variable 'x'
  data <- x$get()
  
  ## Get the inverse of matrix and assign to 'i'
  i <- solve(data, ...)
  
  ## Sets the inverse for the matrix 'x'
  x$setInverse(i)
  
  ## Return a matrix that is the inverse of 'x'
  i
}

