## Put comments here that give an overall description of what your
## functions do

## Write a short comment describing this function
# function makeCacheMatrix will 
# 1. compute the inverse of a square matrix
# 2. stored the inverse value for possible later use

makeCacheMatrix <- function(x = matrix()) {
  im <- NULL
  set <- function(y) {
    x <<- y
    im <<- NULL
  }
  get <- function() x
  setinverse <- function(solve) im <<- solve(x)
  getinverse <- function() im
  list(set = set, 
       get = get,
       setinverse = setinverse,
       getinverse = getinverse)
}

## Write a short comment describing this function
# function cacheSolve will
# 1. retrieve the stored the inverse value if it is stored
# 2. if it is not stored, calculated the inverse
cacheSolve <- function(x, ...) {
  ## Return a matrix that is the inverse of 'x'
  im <- x$getinverse()
  if(!is.null(im)) {
    message("getting cached data")
    return(im)
  }
  data <- x$get()
  im <- solve(data, ...)
  x$setinverse(im)
  im
}
