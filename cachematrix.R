##  These pair of functions cache the inverse of a matrix

## This function creates a special "matrix" object that can cache its inverse
##  1. sets the value of the Matrix
##  2. gets the value of the Matrix
##  3. sets the value of the inverse of the matrix
##  4. gets the value of the inverse of the matrix

makeCacheMatrix <- function(x = matrix()) {
  invmatrix <- NULL
  setmatrix <- function (y){
    x <<- y
    invmatrix <<- NULL
  }
  getmatrix <- function() x
  setinverse <- function(inverse) invmatrix <<- inverse
  getinverse <- function() invmatrix
  list (setmatrix = setmatrix, getmatrix = getmatrix, setinverse = setinverse,
        getinverse = getinverse)
}


## This function computes the inverse of the special "matrix" returned by makeCacheMatrix 
## above. If the inverse has already been calculated (and the matrix has not 
## changed), then the cachesolve should retrieve the inverse from the cache. 

cacheSolve <- function(x, ...) {
        ## Return a matrix nthat is the inverse of 'x'
  invmatrix <- x$getinverse()
  if(!is.null(invmatrix)) {
    message ("Getting cached inverse matrix")
    return(invmatrix)
  }
  matrix <- x$getmatrix()
  invmatrix <- solve(matrix, ...)
  x$setinverse(invmatrix)
  return(invmatrix)
}
