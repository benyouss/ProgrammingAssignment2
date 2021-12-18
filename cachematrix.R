## functions to compute the inverse of matrix and storing the 
## result in the cach

## added comment to test link git-rstudio 

## This function creates a special "matrix" object that can 
## cache its inverse.
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


## This function computes the inverse of the special "matrix"
## returned by makeCacheMatrix. If the inverse has 
## already been calculated (and the matrix has not changed), 
## then the cachesolve should retrieve the inverse from the cache.

cacheSolve <- function(x, ...) {
  inv <- x$getinv()
  # evaluate if the matrix inverse has been already sotored 
  # in the cash
  if(!is.null(inv)) {
    message("getting cached data")
    return(inv)
  }
  
  # compute the inverse and store it in the cash 
  # in the case the matrix is a new one 
  data <- x$get()
  inv <- solve(data, ...)
  x$setinv(inv)
  inv
}
