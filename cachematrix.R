## This is a set of functions to optimize performance of 
## the calculations of matrix's inverse.
## When using cachSolve(), the first time the inverse
## of the matrix is required it will be calculated
## as normal. On subsequent calls, assuming the matrix has
## not changed, it will be retrieved from the cache.

## Creates a wrapper object for a matrix.
## This wrapper has the ability to cache the
## matrix's inverse.

makeMatrix <- function(x = matrix()) 
{
  inv <- NULL
  set <- function(y) 
  {
    x <<- y
    inv <<- NULL
  }
  get <- function() x
  setinverse <- function(inverse) inv <<- inverse
  getinverse <- function() inv
  list(
    set = set, 
    get = get,
    setinverse = setinverse,
    getinverse = getinverse
  )
}

## Computes the inverse of the matrix returned by makeCacheMatrix(). 
## If the inverse has already been calculated and the matrix has not changed, 
## this function will retrieve the inverse from the cache.

cacheSolve <- function(x, ...) 
{
  inv <- x$getinverse()
  if(!is.null(inv)) 
  {
    message("getting cached data")
    return(inv)
  }
  data <- x$get()
  inv <- solve(data, ...)
  x$setinverse(inv)
  inv
}
