# This function is an attempt for the peer-review based excersize for the R
# Programming course in Courera.  This function improves matrix inversion by
# caching the result inverse matrix of this potentially time-consuming
# computation once it has been conputed, so that when we need it again, it can
# be looked up in the cache rather than recomputed.


#This function creates a special "matrix" list object containing a function to
#get/set the value of the matrix and get/set the value of its inverse matrix,
#so that it could cache the value of a matrix along with its inverse matrix.
makeCacheMatrix <- function(x = matrix()) {

  m <- NULL
  set <- function(y) {
    x <<- y
    m <<- NULL
  }
  get <- function() x
  setmean <- function(mean) m <<- mean
  getmean <- function() m 
  list(set = set, get = get,   
       setmean = setmean,
       getmean = getmean)


  # In this assignment, we can assume that the matrix supplied is always invertible.
  # if X is a square invertible matrix, then solve(X) returns its inverse.



}


#This function calculates the inverse of the special "matrix" created with the
#above function: makeCacheMatrix(). However, it first checks to see if the
#inverse has already been calculated for the given matrix. If so, it gets the
#inverse from the cache and skips the computation. Otherwise, it calculates the
#inverse of the input matrix and sets the value of the inverse matrix in the
#cache of the special "matrix" object.
cacheSolve <- function(x, ...) {
  ## Return a matrix that is the inverse of 'x'
  #If the inverse has already been calculated (and the matrix has not changed), then retrieve the inverse from the cache.
  m <- x$getmean()
  if(!is.null(m)) {
    message("getting cached data")
    return(m)
  }
  data <- x$get()
  m <- mean(data, ...)
  x$setmean(m)
  m
}
