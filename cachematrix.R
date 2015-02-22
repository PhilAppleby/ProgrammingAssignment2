# makeCacheMatrix
# make a cached matrix object which is returned as a list of interface methods
# set() - initialise the global-level variables
# get() - get the original matrix
# getinverse() - get the inverse of the original matrix
# setinverse() - set the inverse matrix value
#
makeCacheMatrix <- function(x = matrix()) {
  inv <- NULL
  set <- function(y) {
    x <<- y
    inv <<- NULL
  }
  get <- function() x
  setinverse <- function(inverse) inv <<- inverse
  getinverse <- function() inv
  list(set = set, get = get,
       setinverse = setinverse,
       getinverse = getinverse)
}

# Note that the signature of this function has been changed in order
# that the test for matrix equality makes sense

# Test: 
#   that the data content of x is equal to y 
# AND that 
#   x has an inverse already calculated 
# If so: 
#   return the inverse.
# If not: 
#   run solve 
#   save y into x (via set())
#   save the inverse into x (via setinverse())
#   return the inverse.
#
cacheSolve <- function(x, y, ...) {
  ## Return a matrix that is the inverse of 'x'
  if (all.equal(x$get(), y)) {
    inv <- x$getinverse().
    if(!is.null(inv)) {
      message("getting cached data")
      return(inv)
    }    
  }
  inv <- solve(y, ...)
  x$set(y)
  x$setinverse(inv)
  inv
}
