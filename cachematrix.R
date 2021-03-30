## Put comments here that give an overall description of what your
## functions do

## Write a short comment describing this function: This function creates a special "matrix" object that can cache its inverse.

makeCacheMatrix <- function(p = matrix()) {
  u <- NULL
  set <- function(j) {
    p <<- j
    u <<- NULL
  }
  get <- function() p
  setinverse <- function(inverse) u <<- inverse
  getinverse <- function() u
  list(set = set,
       get = get,
       setinverse = setinverse,
       getinverse = getinverse)
}


## Write a short comment describing this function: This function computes the inverse of the special "matrix" returned by makeCacheMatrix above. If the inverse has already been calculated (and the matrix has not changed), then cacheSolve should retrieve the inverse from the cache.

cacheSolve <- function(p, ...) {
        ## Return a matrix that is the inverse of 'p'
  u <- p$getinverse()
  if (!is.null(u)) {
    message("getting cached data")
    return(u)
  }
  data <- p$get()
  i <- solve(data, ...)
  p$setinverse(u)
  i
}
