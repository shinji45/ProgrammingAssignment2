## Contains functions to store matrix and caches its inverse.

## A function that returns a collection of functions to mutate data within

makeCacheMatrix <- function(x = matrix()) {
  inverse <- NULL
  set <- function(y) {
    x <<- y
    inverse <<- NULL
  }
  
  get <- function() { x
  }
  
  setInverse <- function(inverseInput) {
    inverse <<- inverseInput
  }
  
  getInverse <- function() {
    inverse
  }
  
  list(set = set, get = get, 
       setInverse = setInverse, getInverse = getInverse)
  
}


## Takes in a makeCacheMatrix function, and finds its inverse.
## if inverse already exist, uses it else calculates and stores it.

cacheSolve <- function(x, ...) {
  ## Return a matrix that is the inverse of 'x'
  inverse <- x$getInverse()
  if (!is.null(inverse)) {
    message("Getting cached value")
    return(inverse)
  }
  
  data <- x$get()
  inverse <- solve(data, ...)
  x$setInverse(inverse)
  return(inverse)
}
