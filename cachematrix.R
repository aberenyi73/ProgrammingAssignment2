## This file contains two functions that are used to create a special object 
## that stores a matrix and cache's its inverse.

## This function creates a list() contining functions to: 
## get and set the value of a matrix
## get and set the cached value of the mean of the same matrix
makeCacheMatrix <- function(x = matrix()) {
  inv <- NULL
  get <- function() {x}
  set <- function(y) {
    inv <<- NULL
    x <<- y
  }
  setInverse <- function(inverse) {inv <<- inverse}
  getInverse <- function() {inv}
  list(get = get, 
       set = set, 
       getInverse = getInverse,
       setInverse = setInverse)
}


## This function calculates the inverse of a special "matrix" object created
## by makeCacheMatrix. However, it first checks if the inverse has already been
## calculated by calling getInverse. If it returns null, it will calculate the
## inverse using solve(x) and store the results with setInverse. Otherwise it
## just returns the pre-computed value with getInverse.

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
    inv <- x$getInverse()
    if(is.null(inv)) {
        inv <- solve(x$get(), ...)
        x$setInverse(inv)
    } else {
        print("getting cached value")
    }
    inv
}
