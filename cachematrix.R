## Put comments here that give an overall description of what your
## functions do

## The makeCacheMatrix function creates a special matrix which is a list 
## containing a function to set and get the value of the matrix and to set 
## and get the value of it's inverse

makeCacheMatrix <- function(x = matrix()) {
  m <- NULL
  set <- function(y) {
    x <<- y
    m <<- NULL
  }
  get <- function() x
  setsolve <- function(solve) m <<- solve
  getsolve <- function() m
  list(set = set, get = get,
       setsolve = setsolve,
       getsolve = getsolve)
}


## Returns a matrix that is the inverse of 'x' by checking to see if the 
## inverse already exists. If is does it gets the inverse. If not, it creates  
## the inverse matrix then cache's it

cacheSolve <- function(x, ...) {
  ## Return a matrix that is the inverse of 'x'
  m <- x$getsolve()
  if(!is.null(m)) {
    message("getting cached data")
    return(m)
  }
  data <- x$get()
  m <- solve(data, ...)
  x$setsolve(m)
  m
}
