## These functions create a matrix object that can cache its inverse. 
## To create the matrix m: m <- makeCacheMatrix(x) where x in this case in an ordinary matrix. 
## To return the value m$get()
## To change the value m$set(y) where y is an ordinary matrix.
## To get the inverse with cacheSolve(m)

makeCacheMatrix <- function(x = matrix()) {
      
      cache <- NULL
      set <- function(a) {
            x <<- a
            cache <<- NULL
      }
      get <- function() x
      setInverse <- function(solve) cache <<- solve
      getInverse <- function() cache
      list(set = set, get = get, setInverse = setInverse, getInverse = getInverse)      
}


## 'cacheSolve' calculates the the inverse of the matrix returned by 'makeCacheMatrix' above
## In case the inverse has been calculated, 'cacheSolve' returns the inverse from the cache

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x' 
        
      cache <- x$getinverse()
      if(!is.null(cache)) {
            message("Returning cached data")
            return(cache)
      }
      data <- x$get()
      cache <- solve(data, ...)
      x$setinverse(cache)
      cache
}
