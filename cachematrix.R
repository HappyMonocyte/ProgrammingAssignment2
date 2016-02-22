#R Programming Assignment 2
#Caching the inverse of a matrix
#
#This first function creates a special "matrix"
#that can cache its inverse.

makeCacheMatrix <- function(x = matrix()) {
  i <- NULL
  set <- function(y) {
    x <<- y
    i <<- NULL
  }
  get <- function() x
  setinverse <- function(inverse) i <<- inverse
  getinverse <- function() i
  list(set = set,get = get,
       setinverse = setinverse,
       getinverse = getinverse)
}

#This second function calculates the inverse of
#the special "matrix" created above. It first checks
#whether the inverse has already been calculated, and
#if so it gets the inverse from cache snd skips the 
#calculation. If not, it calculates the inverse.

cacheSolve <- function(x, ...) {
  i <- x$getinverse()
  if (!is.null(i)) {
    message("getting cached data")
    return(i)
  }
  matr <- x$get()
  i <- solve(matr, ...)
  x$setinverse(i)
  i
}
