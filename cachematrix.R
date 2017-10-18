## Put comments here that give an overall description of what your
## functions do

## Write a short comment describing this function

makeCacheMatrix <- function(x = matrix()) {
  inverse <- NULL ## Initializing the value of variable"inverse"
  set <- function(y) {  ## Setting the new matrix value to "x" and reinitializing the "inverse"
    x <<- y
    inverse <<- NULL
  }
  get <- function() x   ## Getting the stored matrix value
  setinverse <- function(solve) inverse <<- solve  ## setting the value of inverse matrix in parent environment
  getinverse <- function() inverse  ## getting the value of inverse matrix
  list(set = set, get = get,
       setinverse = setinverse,   ## Returning four different variables as list
       getinverse = getinverse)

}


## Write a short comment describing this function

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
  inverse <- x$getinverse()  ## Getting the value initially stored in "inverse"
  if(!is.null(inverse)) {
    message("getting cached data")  ## Checking that the value is null or not
    return(inverse)
  }
  data <- x$get()   ## Getting the matrix value in data variable
  inverse <- solve(data, ...) ## Calculating the "inverse" value
  x$setinverse(inverse) ## setting the value of "inverse" in parent environment 
  inverse   ## Returning the "inverse" value
}
