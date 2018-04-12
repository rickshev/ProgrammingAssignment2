## Function that creates special matrix that caches the inverse

makeCacheMatrix <- function(x = matrix()){
  inv <- NULL
  set <- function(y){
    x <<- y
    inv <<- NULL
  }
  get <- function()x
  setInverse <- function() inv <<- solve(x)
  getInverse <- function() inv
  list(set = set, get = get, setInverse = setInverse, getInverse = getInverse)
}

## Produces the inverse matrix of the cached matrix in makeCacheMatrix
## function

cacheSolve <- function(x, ...){
  ## Returns inverse of matrix x
  
  inv <- x$getInverse()
  if(!is.null(inv)){
    message("getting cached data.")
    return(inv)
  }
  data <- x$get()
  inv <- solve(data)
  x$setInverse(inv)
  inv
}
