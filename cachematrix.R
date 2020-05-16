## First, set() clears the previous value of the variable 'matrix'.
## Then, get() returns the value of matrix x.
## Then, setinverse() sets the value of the inverse of matrix x and sets it as the new value of the variable 'matrix'.
## Then, getinverse() returns the value of 'matrix'.

makeCacheMatrix <- function(x = matrix()) {

  matrix <- NULL ## matrix exists, so that we can use <<- on it later
  set <- function(y) {
    x <<- y
    matrix <<- NULL ## clears previous values of matrix
  }
  get <- function() x ##returns matrix
  setinverse <- function(inverse)  matrix<<- inverse ##the new matrix is now set as the inverse of the original matrix
  getinverse<- function() matrix ##returns the new matrix
  list(set = set, get = get,
       setinverse = setinverse,
       getinverse = getinverse) ##lists all the things you just did
}
## If 'matrix' is unchanged, cacheSolve returns the previously-calculated value.
## However, if 'matrix' either changed or the inverse hasn't been calculated yet, cacheSolve calculates it.

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'

  matrix <- x$getinverse()
  if(!is.null(matrix)){ ##if matrix is unchanged, and the value of matrix is not-nonexistant - i.e. it's been calculated previously - this will bring it up without recalculating it
    message("getting cached data")
    return(matrix)
  } ##if we don't already have a value for 'matrix', we proceed to calculate it
  data <- x$get() 
  matrix <- solve(data, ...)
  x$setinverse(matrix)
  matrix
}