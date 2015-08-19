## The following functions compute and cache the inverse of a matrix.
## This is for Programming Assignment 2 in the Coursera Data Science R course.
## Gail Gutowski
## 19 Aug 2015

## USAGE:
## x <- matrix(rnorm(4),nrow=2)       Define an invertible matrix
## sx <- makeCacheMatrix(x)           Create a special matrix for caching
## cacheSolve(sx)                     Compute the inverse of matrix x
## cacheSolve(sx)                     Call the inverse again (retrieved from cache)

## The makeCacheMatrix function creates a matrix for caching.

makeCacheMatrix <- function(x = matrix()) {
  #minv will be the cached inverted matrix
  minv <- NULL
  
  #set the value of the matrix
  set <-function(y) {
    x <<- y
    minv <<- NULL
  }
  
  #get the value of the matrix
  get <- function() x
  
  #set the value of the inverted matrix
  setinverse <- function(inverse) minv <<- inverse
  
  #get the value of the inverted matrix
  getinverse <- function() minv
  
  #Return list of our new functions
  list(set = set, get = get,
       setinverse = setinverse,
       getinverse = getinverse)
}



## The cacheSolve function calculates the inverse of the matrix created in the function 
## unless the inverse already exist in the cache, in which case it retrieves the result.
## This function assumes the matrix is always invertible.

cacheSolve <- function(x, ...) {
  
  #Check if inverse has already been computed and cached.
  minv <-  x$getinverse()
  if (!is.null(minv)) {
    message("Getting cached data.")
    return(minv)
  }
  
  #If the inverse is not in the cache, calculate it
  data <- x$get()
  minv <-solve(data, ...)
  
  #Cache the inverse
  x$setinverse(minv)
  
  #Return the inverse
  minv
}
