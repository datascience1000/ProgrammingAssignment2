## Put comments here that give an overall description of what your
## functions do

## makeCacheMatrix creates a "matrix" that can cache its inverse

## makeCacheMatrix stores four functions:
## set; get; setinv; getinv

## set: changes the matrix stored in the main function
## get: returns the matrix x currently stored in the main function
## setinv: changes the inverse matrix stored in the main function
## getinv: returns the inverse matrix currently stored in main function

makeCacheMatrix <- function(x = matrix()) {
  i <- NULL
   set <- function(y) {
    x <<- y
    i <<- NULL
  }
  get <- function() x
  setinv <- function(inv) i <<- inv
  getinv <- function() i
  
  # the following line stores the four functions:
  list(set = set, get = get,
       setinv = setinv,
       getinv = getinv)

}

## cacheSolve computes the inverse of the "matrix" returned by makeCacheMatrix.
## If the inverse has already been calculated 
## (and the matrix has not changed), 
## then the cachesolve retrieves the inverse from the cache.

## Input of cacheSolve is the object where makeCacheMatrix is stored

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
  i <- x$getinv()
  if(!is.null(i)) {
    message("getting cached data")
    return(i)
  }
  data <- x$get()
  i <- solve(data, ...)
  x$setinv(i)
  i
}

