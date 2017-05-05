## These functions will create a special "matrix" object that can cache its inverse
## and then compute the inverse if the inverse has not already been calculated.
## If the inverse has already been calculated, then the value will be retrieved from
## the cache. These functions assume that the inverse always exsits.

## This function creates a special "matrix" object that can cache its inverse

makeCacheMatrix <- function(x = matrix()) {
  
  #Initialize objects
  inv <- NULL
  
  #Define "getters and setters"
  set <- function(y){
    x <<- y
    inv <<- NULL
  }
  get <- function() x
  setinv <- function(solve) inv <<- solve
  getinv <- function() inv
  
  #Create list object that includes the matrix
  list(set = set, get = get, setinv = setinv, getinv = getinv)
}

## This function computes the inverse of the special "matrix" returned by 
## makeCacheMatrix above. If the inverse has already been calculated (and 
## the matrix has not changed), then the cachesolve should retrieve the inverse 
## from the cache.

cacheSolve <- function(x, ...) {
  ## Return a matrix that is the inverse of 'x'
  
  #Get inverse from list
  inv <- x$getinv()
  
  #If inverse is non-null, then obtain the cached result and do not recalculate
  if(!is.null(inv)){
    message("Getting Cached Data")
    return(inv)
  }
  #If inverse is null, then calculate the inverse (using solve)
  data <- x$get()
  inv <- solve(data, ...)
  
  #Cache the value of the inverse so that it does not need to be recalculated
  x$setinv(inv)
  inv
}
