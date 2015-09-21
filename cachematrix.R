## This pair of functions caches the inverse of a matrix.
## Note that the functions assumes a square invertible matrix
## will be given as input. No checks are made to verify this.

## The function makeCacheMatrix creates a special object
## that can cache its inverse. It makes use of the solve function
## in R to calculate the inverse of a square matrix.

makeCacheMatrix <- function(x = matrix()) {
  s <- NULL
  set <- function(y)  {
    x <<- y
    s <<- NULL
  }
  get <- function() x
  setinverse <- function(solve) s <<-solve
  getinverse <- function() s
  list(set = set, get = get, 
       setinverse = setinverse,
       getinverse = getinverse)
}

## The function cacheSolve returns the inverse of the given matrix.
## If the inverse has already been calculated, and the matrix has not
## been altered, the cached value of the inverse is returned, along
## with a message informing us of this.

cacheSolve <- function(x, ...) {
  ## Return a matrix that is the inverse of 'x'
  s <- x$getinverse()
  if(!is.null(s)) {
    message("getting cached data")
    return(s)
  }
  data <- x$get()
  s <- solve(data, ...)
  x$setinverse(s)
  s
}
