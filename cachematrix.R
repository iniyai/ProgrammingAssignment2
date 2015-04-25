## makeCacheMatrix creates a matrix of the specified size
## cacheSolve finds the inverse of that matrix

## makeCacheMatrix has four functions
## set - which sets the number of rows and columns and the elements of the matrix
## get - get the matrix values along with its dimensions
## setinverse - set the inverse of the matrix (used by cacheSolve)
## getinverse - gets the inverse of the matrix

makeCacheMatrix <- function(x = matrix()) {
  m <- NULL
  set <- function(y) {
    x <<- y
    m <<- NULL
  }
  get <- function() x
  setinverse <- function(mean) m <<- mean
  getinverse <- function() m
  list(set = set, get = get,setinverse = setinverse,getinverse = getinverse)
}

## Find if the matrix inverse already exists
## If it does, return that value
## Else compute that value and store it inside the object.

cacheSolve <- function(x, ...) {
  ## Return a matrix that is the inverse of 'x'
  m <- x$getinverse()
  if(!is.null(m)) {
    message("getting cached data")
    return(m)
  }
  data <- x$get()
  m <- solve(data,...)
  x$setinverse(m)
  m
}
