## Caching results from time-consuming computations enables you to simply
## look them up later instead of computing them again.

## creates a special "matrix" object that can cache its inverse.

makeCacheMatrix <- function(x = matrix()) {
  m = NULL
  set = function(y) {
    x <<- y
    m <<- NULL
  }
  get = function() x
  setmatrix = function(solve) inv <<- solve 
  getmatrix = function() m
  list(set=set, get=get, 
       setmatrix=setmatrix, 
       getmatrix=getmatrix)
}

## computes the inverse of the "matrix" returned by makeCacheMatrix(). 
##If the inverse has already been calculated and the matrix has not changed, 
##the inverse will be retrieved directly from the cache.


cacheSolve <- function(x=matrix(), ...) {
  ## Return a matrix that is the inverse of 'x'
  m = x$getmatrix()
  if (!is.null(m)){
    message("getting cached data")
    return(m)
  }
  
  # otherwise, calculates the inverse 
  matrix = x$get()
  m = solve(matrix, ...)
  
  x$setmatrix(m)
  return(m)
}

