## This pair of functions uses the lexical scoping of R to store the inversion
## of matrices once they have been calculated in order to improve performance.

## This function 'makeCacheMatrix' creates a special 'matrix', which is really
## a list containing a function to:
##
## 1. Set the value of the matrix
## 2. Retrieve the value of the matrix
## 3. Set the value of the inverse matrix
## 4. Get the value of the inverse matrix
makeCacheMatrix <- function(x = matrix()) {
  m <- NULL
  set <- function(y) {
    x <<- y
    m <<- NULL
  }
  get <- function() x
  setinverse <- function(solve) m <<- solve
  getinverse <- function() m
  list(set = set, get = get,
       setinverse = setinverse,
       getinverse = getinverse)
}

## This cacheSolve function uses a matrix created by the makeCacheMatrix function
## to first check whether the matrix inversion was previously calculated. If it had
## been, then the cached value is returned. If the inversion had not previously been
## calculated then it is first calculated, cached, and then returned.
cacheSolve <- function(x, ...) {
  m <- m$getinverse()
  if (!is.null(m)) {
      message("getting cached data")
      return(m)
  }
  data <- x$get()
  m <- solve(data, ...)
  x$setinverse(m)
  m
}
