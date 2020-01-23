## The two functions below helps caching the inverse of a matrix
##

## The first function makeCacheMatrix creates a "new" matrix and provides functions to set and get the matrix and its inverse

makeCacheMatrix <- function(x = matrix()) {
  mat_inv <- NULL

  set <- function(y) {
    x <<- y
    mat_inv <<- NULL
  }

  get <- function() x
  setinv <- function(inv) mat_inv <<- inv
  getinv <- function() mat_inv
  list(set = set, get = get,
       setinv = setinv,
       getinv = getinv)

}

## The second function computes the inverse of a matrix (assumption: it is always invertible)

cacheSolve <- function(x, ...) {
  ## Return a matrix that is the inverse of 'x'
  mat_inv <- x$getinv()
  if(!is.null(mat_inv)) {
    message("getting cached data")
    return(mat_inv)
  }

  data <- x$get()
  mat_inv <- solve(data, ...)
  x$setinv(mat_inv)
  mat_inv

}