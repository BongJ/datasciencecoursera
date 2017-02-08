## Together, these 2 functions 1) create a matrix object (mat) capable of caching 
## its inverse matrix (inv), and 2) return that inverse matrix either by retrieving it
## (if available) or by calculating it

## Creates the matrix object, capable of caching its inverse

makeCacheMatrix <- function(mat = matrix()) {
  inv <- NULL
  set <- function(y) {
    mat <<- y
    inv <<- NULL
  }
  get <- function() mat
  setinv <- function(solve) inv <<- solve
  getinv <- function() inv
  list(set = set, get = get,
       setinv = setinv,
       getinv = getinv)
}


## Retrieves matrix inverse from cache, or calculates it, if unavailable

cacheSolve <- function(mat, ...) {
  inv <- mat$getinv()
  if(!is.null(inv)) {
    message("getting cached data")
    return(inv)
  }
  data <- mat$get()
  inv <- solve(data, ...)
  mat$setinv(inv)
  inv
}
        

