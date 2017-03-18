## Both functions makeCacheMatrix and cacheSolve work together to cache a specific matrix inverse
## to prevent running the inverse again when the function is called. and if the matrix is different 
## than the cached one the a new inverse will be computed and saved in the cahce again together with 
## the new matrix


## makeCacheMatrix is the cache storage function, it creates a list of four sub functions
## set for setting the value of the matrix
## get to retrieve the value of the matrix
## setinv to save the matrix inverse cache
## getinv to get the cached inverse of a matrix

makeCacheMatrix <- function(x = matrix()) {
  inv <- NULL
  set <- function(y) {
    x <<- y
    inv <<- NULL
  }
  get <- function() x
  setinv <- function(inverse) inv <<- inverse
  getinv <- function() inv
  list(set = set, get = get,
       setinv = setinv,
       getinv = getinv)
}


## cacheSolve is where the inverse evaluated, it works togather with makeCacheMatrix.
## x here refers to the object created by makeCacheMatrix function (list of functions and cache). If the inverse has already 
## been calculated (and the matrix has not changed), then the cachesolve should retrieve the inverse from the cache.

cacheSolve <- function(x, ...) {
  ## Return a matrix that is the inverse of 'x'
  inv <- x$getinv()
  if(!is.null(inv)) {
    message("getting cached data")
    return(inv)
  }
  data <- x$get()
  inv <- solve(data,...)
  x$setinv(inv)
  inv
}
