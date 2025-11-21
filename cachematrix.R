## This function creates a special "matrix" object that can cache its inverse.
makeCacheMatrix <- function(x = matrix()) {
  inv <- NULL

  ## 1. Function to set the matrix value
  set <- function(y) {
    x <<- y
    inv <<- NULL
  }

  ## 2. Function to get the matrix value
  get <- function() x

  ## 3. Function to set the cached inverse
  setinverse <- function(inverse) inv <<- inverse

  ## 4. Function to get the cached inverse
  getinverse <- function() inv

  ## Return a list of the methods
  list(set = set, get = get,
       setinverse = setinverse,
       getinverse = getinverse)
}


## This function computes the inverse of the special "matrix" object
## returned by makeCacheMatrix. If the inverse has already been computed
## (and the matrix has not changed), then cacheSolve retrieves the
## inverse from the cache.
cacheSolve <- function(x, ...) {
  ## Try to get the cached inverse
  inv <- x$getinverse()

  ## If a cached inverse exists, return it
  if(!is.null(inv)) {
    message("getting cached data")
    return(inv)
  }

  ## Otherwise, get the matrix data
  data <- x$get()

  ## Compute the inverse using solve()
  inv <- solve(data, ...)

  ## Cache the computed inverse
  x$setinverse(inv)

  ## Return the inverse
  inv
}
