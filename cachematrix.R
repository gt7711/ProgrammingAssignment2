## This script contains two functions. makeCacheMatrix and cacheSolve. The two functions together do the calculation of
## matrix inverse and do cache it.

## The function, makeCacheMatrix creates a special "vector", which is really a list containing a function to

## 1. set the value of the vector
## 2. get the value of the vector
## 3. set the value of the matrix inverse
## 4. get the value of the matrix inverse 

makeCacheMatrix <- function(x = matrix()) {
  inverse <- NULL
  set <- function(y){
    x <<- y
    inverse <<- NULL
  }
  get <- function(){
    x
  }
  setInverse <- function(i){
    inverse <<- i
  }
  getInverse <- function(){
    inverse
  }
  list(set = set,
       get = get,
       setInverse = setInverse,
       getInverse = getInverse)
}


## This function calculates the inverse of the special "vector" created with the above function.
## If the inverse is already calculated, it will be retrieved from the cache.

cacheSolve <- function(x, ...) {
  ## Return a matrix that is the inverse of 'x'
  i <- x$getInverse()
  if(!is.null(i)){
    message("Retrieving inverse from cache:")
    return (i)
  }
  mat <- x$get()
  i <- solve(mat)
  x$setInverse(i)
  i
}
