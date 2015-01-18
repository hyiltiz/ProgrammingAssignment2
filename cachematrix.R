## This function is an attempt for the peer-review based exercise for the R
## Programming course in Coursera.  This function improves matrix inversion by
## caching the result inverse matrix of this potentially time-consuming
## computation once it has been computed, so that when we need it again, it
## can be looked up in the cache rather than recomputed.


## This function creates a special "matrix" list object containing a function
## to get/set the value of the matrix and get/set the value of its inverse
## matrix, so that it could cache the value of a matrix along with its inverse
## matrix.
makeCacheMatrix <- function(x = matrix()) {
  if(missing(x)){
    warning("No input matrix provided to create the special object.
Using 3-by-3 magic square as input for demonstration.")
    x <- matrix(c(8,3,4,1,5,9,6,7,2),3)
    
    message("The magic square used is:")
    print(x)
  }

  inv <- NULL
  ## set/get pair function for the matrix value
  set <- function(y) {
    x <<- y
    ## set the inverse to NULL once the matrix value is changed; the cached
    ## inverse is no longer valid
    inv <<- NULL
  }
  get <- function() x


  ## set/get pair function for the matrix inverse

  ## In this assignment, we can assume that the matrix supplied is always
  ## invertible.  if X is a square invertible matrix, then solve(X) returns
  ## its inverse.
  setinverse <- function(solve) inv <<- solve
  getinverse <- function() inv 

  ## return the special "matrix" object
  list(set = set, get = get,   
       setinverse = setinverse,
       getinverse = getinverse)
}


## This function calculates the inverse of the special "matrix" created with
## the above function: makeCacheMatrix(). However, it first checks to see if
## the inverse has already been calculated for the given matrix. If so, it
## gets the inverse from the cache and skips the computation. Otherwise, it
## calculates the inverse of the input matrix and sets the value of the
## inverse matrix in the cache of the special "matrix" object.
cacheSolve <- function(x, ...) {
  ## Return a matrix that is the inverse of 'x'
  inv <- x$getinverse()

  ## If the inverse has already been calculated (and the matrix has not
  ## changed), then retrieve the inverse from the cache.
  if(!is.null(inv)) {
    message("getting cached data")
    return(inv)
  }

  ## Compute the value if not already cached
  data <- x$get()
  inv <- solve(data, ...)
  x$setinverse(inv)
  inv
}
