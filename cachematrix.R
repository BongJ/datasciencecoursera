## Together, these 2 functions 1) create a matrix object (mat) capable of caching 
## its inverse matrix (inv), and 2) return that inverse matrix either by retrieving it
## (if available) or by calculating it

## This function creates a special matrix capable of caching its inverse.

## set - sets value of the matrix
## get - gets value of the matrix
## setinv - sets the inverse of the matrix
## getinv - gets the value of the matrix

makeCacheMatrix <- function(x = matrix()) {
  inv <- NULL
  set <- function(y) {
    x <<- y
    inv <<- NULL
  }
  get <- function() x
  setinv <- function(solve) inv <<- solve
  getinv <- function() inv
  list(set = set, get = get,
       setinv = setinv,
       getinv = getinv)
}


## This function computes the inverse of the matrix returned by makeCacheMatrix.
## If the inverse has already been calculated, and the matrix has not been changed,
## it will simply retrieve the inverse from the cache.

cacheSolve <- function(x, ...) {
  ## attempt to retrieve inverse from cache
  inv <- x$getinv()  
  ## if inverse is in cache, it is returned  
  if(!is.null(inv)) {
    message("getting cached data")
    return(inv)   
  }
  ## if inverse is not in cache, it is calculated
  data <- x$get()
  inv <- solve(data, ...)  ## calculation of inverse
  x$setinv(inv)
  inv   
}

