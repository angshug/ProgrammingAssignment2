## This script will be used to cache the inverse of a matrix 
## for speeding up calculations. Whenever one needs the value of the inverse
## of a matrix, it will be first checked whether it exists in the cache.
## If not, then the inverse will be computed and also stored in the cache for future use

## The function makeCacheMatrix creates a special "matrix" object 
## which has a list of functions to 
## 1. set the value of the matrix  
## 2.	get the value of the matrix  
## 3.	set the value of the inverse  
## 4.	get the value of the  inverse  

makeCacheMatrix <- function(x = matrix()) {
  m <- NULL
  set <- function(y) {
    x <<- y
    m <<- NULL
  }
  get <- function() x
  setinverse <- function(solve) m <<- solve
  getinverse <- function() m
  list(set = set, get = get,
       setinverse = setinverse,
       getinverse = getinverse)
}


## The function cacheSolve computes  for a matrix's inverse 
## If the inverse has already been calculated 
## then  cachesolve  retrieves the inverse from the cache

cacheSolve <- function(x, ...) {
  ## Check if inverse of matrix x  is present in the cache
  m <- x$getinverse()
  if(!is.null(m)) {
    message("getting cached data")
    return(m)
  }
  ## if not present in cache, use the solve function to obtain the inverse of matrix
  ## and store in cache via the setinverse function
  data <- x$get()
  m <- solve(data, ...)
  x$setinverse(m)
  m
  
}
