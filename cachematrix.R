## Caching the Inverse of a Matr
## Below are functions that are used to create a special object that 
## stores a matrix and caches its inverse.

makeCacheMatrix <- function(x = matrix()) {
    m<-NULL

    set <- function(y){
        x <<- y
    m <<- NULL
  }
  get <- function() x
  
  setInverse <- function(solve) m<<-solve
  getInverse <- function() m
  list(set=set, get= get, setInverse = setInverse, getInverse = getInverse)

}


## Computes the inverse of the matrix
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
  m
}
