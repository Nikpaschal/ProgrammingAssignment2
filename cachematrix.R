## Overall function described here is to 'cache' inverted Matrices

## This first function sets a matrix (x), gets its value  and  
## then sets the value of the inverse of x and gets its value.
## The <<- operator is used to assign a value to an object 
## in an environment that is different from the current environment. 

makeCacheMatrix <- function(x = matrix()) {
  z <- NULL
  set <- function(y) {
    x <<- y
    z <<- NULL
  }
  get <- function() x
  setinverse <- function(inverse) z <<- inverse
  getinverse <- function() z
  list(set = set,
       get = get,
       setinverse = setinverse,
       getinverse = getinverse)
}

## Next function firtsly checks whether the inverse of x has 
## been computed. If so, it gets it from cache and skips computation. 
## Otherwise, it calculates the inverse of x (using Solve()) 
## and sets the value of the inverse in the cache via the setinverse function.


cacheSolve <- function(x, ...) {
  z <- x$getinverse()
  if (!is.null(z)) {
    message("getting cached data")
    return(z)
  }
  data <- x$get()
  z <- solve(data, ...)
  x$setinverse(z)
  z
}
