## Put comments here that give an overall description of what your
## functions do

## makeCacheMatrix creates a list of functions to get the matrix, 
## set the matrix, get the inverse of the matrix and set the inverse of the matrix

makeCacheMatrix <- function(x = matrix()) {
    inv <- NULL
    set <- function(y) {
      x <<- y
      inv <<- NULL
    }
    get <- function() x
    setinverse <- function(inverse) inv <<- inverse
    getinverse <- function() inv
    list(set=set, get=get, setinverse=setinverse, getinverse=getinverse)
}


## cacheSolve returns the inversed matrix derived from the above list of
## matrix functions, if it exists; else it calculates and returns the inversed
##matrix, and sets the inversed matrix in the list of matrix functions


cacheSolve <- function(x, ...) {
  inv <- x$getinverse()
  if(!is.null(inv)) {
    message("getting cached data.")
    return(inv)
  }
  data <- x$get()
  inv <- solve(data)
  x$setinverse(inv)
  inv
}
