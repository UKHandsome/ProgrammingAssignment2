## Put comments here that give an overall description of what your
## functions do

##Matrix inversion is high costly computation. If we cache or keep result of matrix inversion,
##It will be benefit and very fast computation. (No need to compute again)
##These functions in this file will provide caching and matrix inversion function.
##  1. Caching Function name as "makeCacheMatrix"
##  2. Matrix inversion name as "cacheSolve"

## Write a short comment describing this function
## Cached matrix is inversed Matrix.
##
## First function is used for cache matrix. You can cached matrix by function "setinv".
##And get cached matrix by function "getinv"



makeCacheMatrix <- function(x = matrix()) {
  
  m <- NULL
  set <- function(y) {
    x <<- y
    m <<- NULL
  }
  get <- function() x
  setinv <- function(mean) m <<- mean
  getinv <- function() m
  list(set = set, get = get,
       setinv = setinv,
       getinv = getinv)
  
  
}


## Write a short comment describing this function
## Second function is used for matrix inversion.
## Comment describe how it work.
## 1) It will check cache before compute inversed matrix.
## 2) If cache are found , It use value from cache.
## 3) If not found, It compute inversed matrix.


cacheSolve <- function(x, ...) {
  ## Return a matrix that is the inverse of 'x'
  
  m <- x$getinv()
  if(!is.null(m)) {
    message("getting cached data")
    return(m)
  }
  data <- x$get()
  m <- solve(data)
  x$setinv(m)
  m
}

