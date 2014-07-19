
#In the following we define two functions dealing with matrices. 
#The scope is to avoid  multiple computations of the inverse of a 
#given invertible  matrix

#The function makeCacheMatrix set a possible empty matrix X in the function 
#environment and return a list of 4 functions  to set and get  the matrix and its inverse  
makeCacheMatrix <- function(X = matrix()) {
  invX <- NULL
  set <- function(Y) {
    X <<- Y
    invX <<- NULL
  }
  get <- function() X
  setinverse <- function(inv) invX <<- inv
  getinverse <- function() invX
  list(set = set, get = get,
       setinverse = setinverse,
       getinverse = getinverse)
}

#The function cachesolve computes the inverse of a matrix as long as the
#inverse is not already in the cache. If this happens the function print the message
#getting cached data
cacheSolve <- function(X, ...) {
  invX <- X$getinverse()
  if(!is.null(invX)) {
    message("getting cached data")
    return(invX)
  }
  data <- X$get()
  invX <- solve(data, ...)
  X$setinverse(invX)
  invX
}