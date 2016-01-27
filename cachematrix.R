## Caching the Inverse of a Matrix
## Below are functions that are used to create a special object that 
## stores a matrix and caches its inverse.

makeCacheMatrix <- function(x = matrix()) {
    m<-NULL

    set <- function(y){
        x <<- y
    m <<- NULL
  }
  ##stores the matrix
  get <- function() x
  ## caches its inverse.
  setInverse <- function(solve) m<<-solve
  getInverse <- function() m
  list(set=set, get= get, setInverse = setInverse, getInverse = getInverse)

}

## Write a short comment describing this function
## cacheSolve takes the functions list returned by makeCacheMatrix
## and returns the inverse of the matrix - checking and returning 
## the inverse if it has been cached, or solving for the inverse 
## and caching the inverse. The inverse is returned in either case.
##
cacheSolve <- function(x, ...) {
  ## Return a matrix that is the inverse of 'x'
  m <- x$getInverse()
  if(!is.null(m)){
    message("getting cached data")
    return(m)
  }
  data <- x$get()
  m <- solve(data, ...)
  x$setInverse(m)
  ## Return the inverse of the matrix 
  m
}
