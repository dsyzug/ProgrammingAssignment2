
## Function to create a list containing a matrix and its inverse which can be set or get by calling the setinverse
## or getinverse methods.
makeCacheMatrix <- function(x = matrix()) {
  inverse <- NULL
  # use set to replace x by another matrix y if required 
  set <- function(y) {
    x <<- y
    inverse <<- NULL
  }
  get <- function() x
  # set inverse function to set the inverse
  setinverse <- function(inv) inverse <<- inv
  # function to return the inverse
  getinverse <- function() inverse
  list(set = set, get = get,
       setinverse = setinverse,
       getinverse = getinverse)
}


## Takes a cache matrix x (created using makeCacheMatrix) as input parameter
## and returns the cached inverse, if already stored.
cacheSolve <- function(x, ...) {
  # get the inverse of x
  inv <- x$getinverse() 
  # if inverse is already available, return it
  if (!is.null(inv)){
    message("getting cached inverse")
    return(inv)
  }
  # cache not available so need to compute the inverse and set it in cache matrix m for future servings.
  data <- x$get()
  inv <- solve(data)
  x$setinverse(inv)
  inv
}

## Test Case
m <- rbind(c(1,2,3), c(4, 5, 6), c(7, 8, 8)) # a test matrix
cm <- makeCacheMatrix(m) # cache matrix
cacheSolve(cm) # inverse is computed
cacheSolve(cm) # inverse is served from the cache 
