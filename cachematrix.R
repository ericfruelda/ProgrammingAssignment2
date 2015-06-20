## This function creates a special "matrix" object that caches its inverse.

makeCacheMatrix <- function(x = matrix()) {
  ## Create a special matrix object to cache its inverse.
  inv <- NULL
  set <- function(y) {
    x <<- y
    inv <<- NULL
  }
  get <- function() x
  setinv <- function(solve) inv <<- solve
  getinv <- function() inv
  ## Create the list of the inverse
  list(set = set, get = get,
       setinv = setinv,
       getinv = getinv)
}


## This function computes the inverse of the special "matrix"
## returned by makeCacheMatrix. If the inverse has already been
## calculated (and the matrix has not changed), this function
## retrieves the inverse from the cache.

cacheSolve <- function(x, ...) {
  ## Check if the inverse in the cache
  inv <- x$getinv()
  if(!is.null(inv)) {
    message("getting cached data")
  ## Return the inverse from the cache
    return(inv)
  }
  data <- x$get()
  ## Compute the inverse of the matrix
  inv <- solve(data, ...)
  x$setinv(inv)
  ## Return a matrix that is the inverse of 'x'
  inv
}
