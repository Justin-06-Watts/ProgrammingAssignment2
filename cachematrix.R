##  This function creates a special "matrix" object that can cache its inverse.

makeCacheMatrix <- function(x = matrix()) {
	inv <- NULL
	set <- function(y) {
      	x <<- y
            inv <<- NULL
      }
      get <- function() x
      setInv <- function(inverse) inv <<- inverse
      getInv <- function() inv
      list(set = set, get = get,
           setInv = setInv ,
           getInv = getInv)
}


## This function computes the inverse of the special matrix created by the makeCacheMatrix
## above. If the inverse has been calculated, then cacheSolve should retrieve it from cache.

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
	inverse<- x$getInv()
      if(!is.null(inverse)) {
      	message("getting cached data")
            return(inverse)
      }
      data <- x$get()
      inverse <- solve(data,...)
      x$setInv(inverse)
      inverse
}
