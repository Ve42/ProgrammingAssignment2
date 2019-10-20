## Caching the inverse of a matrix

#Creates a matrix object to cache the inverse of a matrix

makeCacheMatrix <- function(x = matrix()) {
  inv <- NULL
  set <- function(y){
    x <<- y
    m <<- NULL
  }
  get <- function() x
  setinv <- function(inverse) inv <<- inverse
  getinv <- function() inv
  list(set = set, get = get, setinv = setinv, getinv = getinv)
}


## Take created matrix and calculate/extract inverse

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
  inv <- x$getinv()
  if(!is.null(inv)){
    message ("searching in cached data...")
    return (inv)
  }
  mat <- x$get()
  inv <- solve(x,...)
  x$setinv(inv)
  inv
}
