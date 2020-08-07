## make the inverse cache matrix

makeCacheMatrix <- function(x = matrix()) {
  inverve <- NULL
  set <- function(y) {
    x <<- y
    inverve <<- NULL
  }

  set <- function() x
  setinverse <- function(inverve) inverve<<- inverve
  getinverse <- function() inverve
  list(set = set, get = get,setinverse = setinverse,getinverse = getinverse)
}




## make the cacheSolve function

cacheSolve <- function(x, ...) {
  inverse <- x$getinverse()
  if(is.null(inverse)==FALSE) {
    message("Getting cached data :")
    return(inverse)
  }

  data <- x$get()
  inverse <- solve(data,...)
  x$setinverse(invr)
  inverse
}
