## makeCacheMatrix:This function creates a special "matrix" object that can cache its inverse.
## input: a matrix
## output: list of functions for the special matrix

makeCacheMatrix <- function(x = matrix()) 
{
    i <- NULL
    set <- function(y) 
      {
      x <<- y
      i <<- NULL
      }
    get <- function() x
    setinverse <- function(mInverse) i <<- mInverse
    getinverse <- function() i
    list(set = set, get = get,
         setinverse = setinverse,
         getinverse = getinverse)
}


## cacheSolve:This function computes the inverse of the special "matrix" returned by makeCacheMatrix above. If the inverse has already been calculated (and the matrix has not changed), 
## then the cachesolve should retrieve the inverse from the cache.
## input: a matrix
## output: a matrix that is the inverse of 'x'

cacheSolve <- function(x, ...)
{
  inv <- x$getinverse()
  if(!is.null(inv)) 
    {
      message("getting cached data")
      return(inv)
    }
  data <- x$get()
  inv <- solve(data)
  x$setinverse(inv)
  return(inv)
  
}