## This script is comprised of 2 functions:  makeCacheMatrix and cachesolve.
## makeCacheMatrix: This function creates a special "matrix" object that can 
## cache its inverse.
## cacheSolve: This function computes the inverse of the special "matrix" 
## returned by makeCacheMatrix. If the inverse has already been calculated 
## (and the matrix has not changed), then the cachesolve will retrieve the 
## inverse from the cache.


makeCacheMatrix <- function(x = matrix()) {
  ## This function creates a special matrix that can cache it's inverse.  
  ## Input parameter X is the matrix to be inverted.  
  ## Function returns a list with 4 items, which are functions to:
  ##      set the matrix value
  ##      get the value of the matrix
  ##      set the value of the inverse
  ##      get the value of the inverse
  ## The "solve" function is used to do the matrix inversion.
  
    m <- NULL
    set <- function(y) {                    ##set the value of the matrix
    x <<- y
    m <<- NULL
  }
  get <- function() x                        ##get the matrix
  setInverse <- function(solve) m <<- solve  ##calculate the inverse by calling solve
  getInverse <- function() m                 ##get the value of the inverse
  
  list(set = set, get = get,                 #(set/return list)
       setInverse = setInverse, 
       getInverse = getInverse)
  
}


cacheSolve <- function(x, ...) {
  ## Return a matrix that is the inverse of 'x'
  ## This function computes the inverse of the special "matrix" returned by 
  ## makeCacheMatrix. If the inverse has already been computed and cached, it
  ## will simply return the re-calculated invers.

  m <- x$getInverse()    ##check the cache
  if(!is.null(m)){       ##if the value of m is not null there is a cache
    message("getting cached data")
    return(m)            ##just return the cached inverse
  }
  
  data <- x$get()        ##inverse not cached
  m <- solve(data, ...)  ##compute the inverse using the solve function
  x$setInverse(m)        ##set the value into the cache
  
}
