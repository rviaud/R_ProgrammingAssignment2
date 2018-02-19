## Put comments here that give an overall description of what your
## functions do

## Write a short comment describing this function

makeCacheMatrix <- function(x = matrix()) {
  ## makeCacheMatrix implements a Matrix object 
  ## that can cache the Inverse Matrix returned by solve()
  ##
  inv <- NULL
  set <- function(y) {  ## sets a new matrix
    x <<- y        ## loads a matrix to be used for solve()
    inv <<- NULL   ## Current solve() response is null to indicate it has not been cached
  }
  get <- function() x  ## gets current matrix
  setinverse <- function(inverse) inv <<- inverse ## sets the solve() result
  getinverse <- function() inv  ## gets the current solve() result cached, null if not
  list(set = set, get = get,
       setinverse = setinverse,
       getinverse = getinverse)
}


## Write a short comment describing this function

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
        ## using a cached value if available
  inv <- x$getinverse()
  if(!is.null(inv)) {   ## If getinverse() not null it is a cached result
    message("getting cached data")
    return(inv)
  }
    ## if getinverse is null we need to calculate solve() and store the value
  data <- x$get()  
  inv <- solve(data, ...)
  x$setinverse(inv)
  inv
}
