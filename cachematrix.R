## Two functions that cache the inverse of a matrix and computes the inverse.

## The function makeCacheMatrix creates a special matrix that can cache its inverse.

makeCacheMatrix <- function(x = matrix()) {
  inv <- NULL
  set <- function(y){
    x <<- y
    inv <<- NULL
  }
  get <- function() x
  setInv <- function(solveMatrix) inv <<- solveMatrix
  getInv <- function() inv
  list(set = set, get = get, 
       setInv = setInv, 
       getInv = getInv)
}

## The function cacheSolve computes the inverse of the special matrix returned by makeCacheMatrix above.

cacheSolve <- function(x, ...) {
  ## Return a matrix that is the inverse of 'x'
  inv <- x$getInverse()
  ## Check if inverse has already been calculated and gets inverse from cache.
  if(!is.null(inv)){
    message("getting cached data")
    return(inv)
  }
  ## Calculate the inverse of the data and sets the value of the inverse via the setInv function
  data <- x$get()
  inv <- solve(data)
  x$setInv(inv)
  inv      
}
