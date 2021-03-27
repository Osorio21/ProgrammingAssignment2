## This pair of functions caches the inverse of a matrix in order to reduce
## computation time.

## This function creates a special matrix through a list of functions designed
## to get/set the matrix value and get/set the inverse value.

makeCacheMatrix <- function(m = matrix()) {
  i <- NULL
  set <- function(y) {
    m <<- y
    i <<- NULL
  }
  get <- function() m
  setinverse <- function(inverse) i <<- inverse
  getinverse <- function() i
  list(set = set, get = get,
       setinverse = setinverse,
       getinverse = getinverse)
}



## This function calculates the inverse of the matrix created with the first
## function.

cacheSolve <- function(m, ...) {
        ## Return a matrix that is the inverse of 'm'
  i <- m$getinverse()
  if(!is.null(i)) {
    message("getting cached data")
    return(i)
  }
  data <- m$get()
  i <- solve(data, ...)
  m$setinverse(i)
  i
}
