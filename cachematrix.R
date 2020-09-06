## Cache the inverse of a matrix.

## Creates a matrix to chache its inverse.
makeCacheMatrix <- function(x = matrix()) {
  inverse_m <- NULL
  set <- function(y){
    x <<- y
    inverse_m <<- NULL
  }
  get <- function() x
  setinverse <- function(solve) inverse_m <<- solve
  getinverse <- function() inverse_m
  list(set = set, 
       get = get,
       setinverse = setinverse,
       getinverse = getinverse)  
}

## Computes the inverse of the matrix. If the inverse has already been calculated, it retunr from cache.
cacheSolve <- function(x, ...) {
    inverse_m <- x$getinverse() 
    if(!is.null(inverse_m)) {
      message("getting cached data")
      return(inverse_m)
    }
    data <- x$get()
    inverse_m <- solve(data, ...)
    x$setinverse(inverse_m)
    inverse_m
}
