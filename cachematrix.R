## The following function must be used for determining the inverse of a supplied matrix
## The results are cached, in a way that if the supplied matrix doesn't change after the its previous
## then the result are retrieved from cache.

## This function creates a "matrix" object that can cache its inverse.

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


## The function below computes the inverse of the special "matrix" returned by makeCacheMatrix 
## and returns the inverse of the matrix passed for argument x.

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
  m <- x$getinverse()
  # If the inverse matrix is already calculated, retrieves it from cache
  if(!is.null(m)) {
    message("getting cached data")
    return(m)
  }
  data <- x$get()
  m <- solve(data, ...)
  x$setinverse(m)
  m  
}
