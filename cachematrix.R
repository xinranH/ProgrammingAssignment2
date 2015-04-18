## By feeding a matrix, the function can calculate the inverse of it quickly With the help of 'special' matrix,
## the recalculation is avoided because the result would be found in the cache.

## It uses the same method as the example, I only change the 'mean' function to 'solve' function.  Setting the
## matrix x, decomposing the matrix into a list, and store it in the cache.

makeCacheMatrix <- function(x = matrix()) {
  s <- NULL
  set <- function(y) {
    x <<- y
    m <<- NULL
  }
  get <- function() x
  setsolve <- function(solve) s <<- solve
  getsolve <- function() s
  list(set = set, get = get, setsolve = setsolve, getsolve = getsolve)
}


## If the inverse of the matrix is already calculated, the function is stopped and print 'getting cached data'.
## If the inverse isn't calculated, the function gives the inverse and sets the inverse in the cache.

cacheSolve <- function(x, ...) {
  ## Return a matrix that is the inverse of 'x'
  s <- x$getsolve()
  if (!is.null(s)) {
    message("getting cached data")
    return(s)
  }
  data <- x$get()
  s <- solve(data, ...)
  x$setsolve(s)
  s
}