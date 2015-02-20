## Matrix inversion is usually a costly computation and there may be some benefit to caching the inverse of a matrix rather than compute it repeatedly (there are also alternatives to matrix inversion that we will not discuss here). Your assignment is to write a pair of functions that cache the inverse of a matrix.
## 
## Write the following functions:
##   
##  makeCacheMatrix: This function creates a special "matrix" object that can cache its inverse.
##  cacheSolve: This function computes the inverse of the special "matrix" returned by makeCacheMatrix above.
##  If the inverse has already been calculated (and the matrix has not changed), then the cachesolve should 
##  retrieve the inverse from the cache.  ## Computing the inverse of a square matrix can be done with the 
##  solve function in R. For example, if X is a square invertible matrix, then solve(X) returns its inverse.
## 
## For this assignment, assume that the matrix supplied is always invertible.
## 
## This function follows the example cachemean() very closely but solves (finds the inverse of) a matrix instead
## of the mean of a vector.  Note I have inlcuded, commented out, a test=wrapper() function at the bottom of this file.
## I always write a test routine for any function I write to help rapidly and repeatedly test and debug the function.

makeCacheMatrix <- function(x = matrix()) {
  inv <- NULL
  set <- function(y) {
    x <<- y
    inv <<- NULL
  }
  get <- function() x
  setinverse <- function(solve) inv <<- solve
  getinverse <- function() inv
  list(set = set, get = get,
       setinverse = setinverse,
       getinverse = getinverse)
}

cacheSolve <- function(x, ...) {
  inv <- x$getinverse()
  if(!is.null(inv)) {
    message("getting cached data")
    return(inv)
  }
  data <- x$get()
  inv <- solve(data, ...)
  x$setinverse(inv)
  inv
}
# testwrapper() sets up a solvavle matrix, calls makeCacheMatrix() and then cacheSolve() before 
# printing out the inverse of the original data matrix.  testwrapper2() does the same but calls
# cacheSolve() twice to demonstrate the solution is pulled from the cache if it already has been stored there,
#
# testwrapper<-function(){
#   testmatrix <- matrix(c(7,0,-3,2,3,4,1,-1,-2), nrow=3, ncol=3)
#   invtest <-makeCacheMatrix(testmatrix)
#   invmatrix <-cacheSolve(invtest)
#   return(invmatrix)
# }
# testwrapper2<-function(){
#   testmatrix <- matrix(c(7,0,-3,2,3,4,1,-1,-2), nrow=3, ncol=3)
#   invtest <-makeCacheMatrix(testmatrix)
#   invmatrix <-cacheSolve(invtest)
#   invmatrix <-cacheSolve(invtest)
#   return(invmatrix)
# }