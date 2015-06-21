#These functions, in conjunction, find the inverse of a matrix, and store it in cache.  If the inverse of the matrix is already stored in cache, the value is pulled from cache rather than being computed again. 

#This function creates a "matrix", which is actually a list of functions that do the following:
#  1. set the value of the matrix
#	2. get the value of the matrix
#	3. set the value of the inverse of the matrix
#	4. get the value of the inverse of the matrix

makeCacheMatrix <- function(x=matrix()) {
  i <- NULL
  set <- function(y) {
    x <<- y
    i <<- NULL
  }
  get <- function() x
  setinv <- function(inverse) i <<- inverse
  getinv <- function() i
  list(set = set, get = get, 
       setinv = setinv,
       getinv = getinv)
}

#This function calculates the inverse of the "matrix" from the previous function. However, it first checks to see if the inverse has already been calculated.  If so, it returns the inverse stored in cache, rather than computing it again, and prints a message letting the user know this happened. If not, it returns the computed value, and sets the value of the inverse in cache to the computed value.

cacheSolve <- function(x, ...) {
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