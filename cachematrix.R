## Matrix inversion is usually a costly computation and there may be some benefit to caching the inverse of a matrix rather than compute it repeatedly. 
## Here are a pair of functions that cache the inverse of a matrix. 

## Setting function that generates inverse of a matrix, and stores its inverse
## Creates a special "matrix" object that can cache its inverse.

makeCacheMatrix <- function(x = matrix()) {
  m <- NULL
  set <- function(y) {
    x <<- y
    m <<- NULL
  }
  get <- function() x
  setinv <- function(solve) m <<- solve
  getinv <- function() m
  list(set = set, get = get,
       setinv = setinv,
       getinv = getinv)
}



## Call inverse of matrix from cache
## This function computes the inverse of the special "matrix" returned by makeCacheMatrix above. If the inverse has already been calculated.

cacheSolve <- function(x, ...) {
  m <- x[getinv()]
  if(!is.null(m)) {
    message("getting cached data")
    return(m)
  }
  data <- x$get()
  m <- solve(data, ...)
  x$setinv(m)
  m
}
