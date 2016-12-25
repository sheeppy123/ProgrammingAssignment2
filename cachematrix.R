## This is the solutions for assignment two 
## including two new functions MakeCacheMatrix and cacheSolve

## This funtion creates a special "matrix" object that can cache its inverse

makeCacheMatrix <- function(x = matrix()) {
  m <- NULL
  set <- function(y) {
    x <<- y
    m <<- NULL
  }
  get <- function() x
  setInv <- function(inverse) m <<- inverse
  getInv <- function() m
  list(set = set,
       get = get,
       setInv = setInv,
       getInv = getInv)
}


## This function computes the inverse of the special "matrix" created by 
## makeCacheMatrix above. If the inverse has already been calculated (and the matrix has not changed), 
##then cacheSolve should retrieve the inverse from the cache.
cacheSolve <- function(x, ...) {
  m <- x$getInv()
  if (!is.null(m)) {
    message("getting cached data")
    return(m)
  }
  m1 <- x$get()
  m <- solve(m1, ...)
  x$setInv(m)
  m
}