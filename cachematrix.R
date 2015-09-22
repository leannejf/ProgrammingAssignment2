## Used together, these two functions cache the inverse of a matrix such that the inverse will 
## only be computed if it hasn't been already or if the matrix has changed.

## makeCacheMatrix takes a matrix object and creates a list containing functions 
## that will assign or get the value of the matrix and will set or get the 
## inverse of the matrix.

makeCacheMatrix <- function(x = matrix()) {
      ## Set matrix 'x' and create a list of 4 functions.
      m <- NULL
      set <- function(y) {
            x <<- y
            m <<- NULL
      }
      get <- function() x
      setinverse <- function(solve) m <<- solve
      getinverse <- function() m
      list(set = set, get = get, setinverse = setinverse, getinverse = getinverse)
}

## cacheSolve checks to see if the inverse of the matrix set by makeCacheMatrix has 
## already been calculated. It retrieves the inverse or calculates and caches
## the inverse if it has not been calculated previously.

cacheSolve <- function(x, ...) {
      ## Return a matrix that is the inverse of 'x'
      m <- x$getinverse()
      if(!is.null(m)) {
            message("getting cached inverse")
            return(m)
      }
      data <- x$get()
      m <- solve(data, ...)
      x$setinverse(m)
      m
}