# 1. Function to creates a special matrix object that can cache its inverse
makeCacheMatrix =function( M = matrix() ) {
  
  # Assign the inverse property
  in =NULL
  
  # Method to set the matrix
  set = function( matrix ) {
    M <<- matrix
    in <<- NULL
  }
  
  # To get the matrix
  get <- function() {
    # Return the matrix
    M
  }
  
  # To set the inverse of the matrix
  Inversematrix <- function(inverse) {
    in <<- inverse
  }
  
  # To get the inverse of the matrix
  getInverse <- function() {
    in
  }
  
  ## Return a list of the methods
  list(set = set, get = get,
       Inversematrix = Inversematrix,
       getInverse = getInverse)
}

#2.
# Compute the inverse of the special matrix returned by "makeCacheMatrix"
# above. If the inverse has already been calculated (and the matrix has not
# changed), then the "cachesolve" should retrieve the inverse from the cache.

cacheSolve <- function(x, ...) {
  
  # Return a matrix that is the inverse of 'x'
  M <- x$getInverse()
  
  # Just return the inverse if its already set
  if( !is.null(M) ) {
    message("getting cached data")
    return(M)
  }
  
  ## Get the matrix from our object
  data <- x$get()
  
  ## Calculate the inverse using matrix multiplication
  M <- solve(data) %*% data
  
  ## Set the inverse to the object
  x$Inversematrix(M)
  
  M
}
#example
makeCacheMatrix(matrix(c(1,2,3,4), nrow=2))
