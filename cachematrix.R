## Matrix inversion is usually a costly computation and there may be
## some benefit to caching the inverse of a matrix rather than computing
## it repeatedly. Here a pair of functions that cache the inverse of a matrix.

## This function creates a special "matrix" object that can cache its inverse
makeCacheMatrix <- function(x = matrix()) {
  ## variable for matrix inverse
  xinv <- NULL
  set <- function(y) {
    ## when ever matrix is changed, inverse is set to NULL to re-compute again
    x <<- y
    xinv <<- NULL
  }
  get <- function() x
  setinverse <- function(inverse) xinv <<- inverse
  getinverse <- function() xinv
  
  list(set = set, get = get,
       setinverse = setinverse, getinverse = getinverse
  )
}


## This function computes the inverse of the special "matrix" returned by
## makeCacheMatrix above. If the inverse has already been calculated
## (and the matrix has not changed - inverse is set to NULL in this case),
## then cacheSolve should retrieve the inverse from the cache.

cacheSolve <- function(x, ...) {
  ## retrive the matrix inverse from the special matrix
  xinv <- x$getinverse()
  
  ## if inverse is already present fetch it else compute and set into cache
  if (!is.null(xinv)) {
    return(xinv)
  }
  data <- x$get()
  xinv <- solve(data, ...)
  x$setinverse(xinv)
  xinv
}