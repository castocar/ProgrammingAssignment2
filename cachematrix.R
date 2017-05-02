## This assignment is to write a pair of functions that 
## cache the inverse of a matrix

## makeCacheMatrix: creates a special "matrix" object that can cache its inverse

makeCacheMatrix <- function(x = matrix()) {
  m <- NULL
  set <- function(y) {
    x <<- y
    m <<- NULL
  }
  get <- function() x
  setinv <- function(inv) m <<- inv
  getinv <- function() m
  list(set = set, get = get, setinv = setinv, getinv = getinv)
}
## cacheSolve: computes the inverse of the special "matrix" 
## If the inverse has already been calculated, and the matrix has not changed, 
## then the cachesolve should retrieve the inverse from the cache

cacheSolve <- function(x = matrix(), ...) {
  m <- x$getinv()
  if(!is.null(m)) {
    message("getting inverse matrix")
    return(m)
  }
  data <- x$get()
  m <- solve(data, ...)
  x$setinv(m)
  ## Return a matrix that is the inverse of 'x'  
  m        
}

