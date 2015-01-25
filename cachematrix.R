## The two functions below create a special matrix
## object that store a matrix and cache its inverse,
## thereby saving time on potentially time-consuming
## computations.

## This function creates a matrix object.
## It contains a list with functions that
## set and get the value of the matrix, 
## and set and get the value of its inverse. 
makeCacheMatrix <- function(x = matrix()) {
  m <- NULL
  
  ## set function is to set matrix
  ## NOTE: The <<- operator assigns a 
  ## value to an object in a different environment 
  ## from the one in which it was defined. 
  set <- function(y) {
    x <<- y
    m <<- NULL
  }
  
  ## function to get matrix that has been set
  get <- function() {
    x
  }
  
  ## function to solve inverse of matrix
  setinverse <- function(solve) {
    m <<- solve
  }
  
  ## function to get inverse of matrix
  getinverse <- function() {
    m
  }
  
  ##returns these functions so we can access them
  list(
    set = set, 
    get = get,
    setinverse = setinverse,
    getinverse = getinverse
  )
}

## The following function calculates the inverse of a 
## matrix. First, it checks to see if the inverse has
## already been calculated. If so, it prints a message
## and skips the calculation. If not, it calculates
## the inverse of the matrix and sets its inverse value
## by calling the setinverse function from the 
## makeCacheMatrix function (see above).

cacheSolve <- function(x=matrix(), ...) 
{ 
  ## returns inverse of x matrix if already been set.
  m <- x$getinverse() 
  
  if(!is.null(m)) 
    ## checks if m has been calculated. Will only be
    ## null if inverse has not yet been calculated. 
    ## In which case, it skips the message below and 
    ## calculates the inverse via setinverse function
    ## below. 
  {
    message("getting cached data")
    return(m)
  }
  
    ## if matrix hasn't been solved, gets the matrix
  matrix <- x$get()
    ## solves the matrix.
  m <- solve(matrix, ...)
    ## sets inverse by calling setinverse function
    ## from matrix object defined above. 
  x$setinverse(m)
    ## returns inverse. 
  m
}
