## Put comments here that give an overall description of what your
## functions do

## mackeCacheMatrix is a function that contains a list of functions that can be used to 
##  invert a matrix and store it in a cache

makeCacheMatrix <- function(x = matrix()) {

  mx <- NULL
  set <- function(y) {
    x <<- y
    mx <<- NULL
  }
  get <- function() x
  setSolve <- function(solve) mx <<- solve
  getSolve <- function() mx
  list(set = set, get = get,
       setSolve = setSolve,
       getSolve = getSolve)
  
}


## cacheSolve will return the inverse of a matrix that is contained within a makeCacheMatrix 
##   argument that is passed in.  If the cache is not null, it will return the cache data. 
##   Otherwise, it will use the Solve function to invert the matrix
cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
  mx <- x$getSolve()
  if(!is.null(mx)) {
    message("getting cached data")
    return(mx)
  }
  data <- x$get()
  mx <- solve(data, ...)
  x$setSolve(mx)
  mx
}
