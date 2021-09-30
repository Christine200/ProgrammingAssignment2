## Description: Create matrix that can cache its inverse


## makeCacheMatrix 

# arguments:
# x: a matrix (optional)

# return:
# a matrix with functions to get/set value and get/set inverse


makeCacheMatrix <- function(x = matrix()) {
  # cached inverse of matrix
  inverse <- NULL
  
  ## get/set matrix
  get <- function() x
  set <- function(y) {
    x <<- y
    inverse <<- NULL
  }
  
  ## get/set matrix inverse
  getinverse <- function() inverse
  setinverse <- function(inversem) inverse <<- inversem
  
  ## return list of functions for matrix
  list(get=get, set=set, getinv=getinverse, setinverse=setinverse)
}


## cacheSolve

# arguments:
# x: a matrix
# ...: extra arguments

# return:
# inverse of the matrix


cacheSolve <- function(x, ...) {
  inverse <- x$getinv()

  # return cached matrix inverse if it has already been calculated
  if (!is.null(inverse)) {
    message("inverse is cached")
    return(inverse)
  }
  
  # compute inverse of matrix 
  matr <- x$get()
  inverse <- solve(matr, ...)
  
  # cache inverse
  x$setinverse(inverse)
  
  # return inverse of matrix
  return(inverse)
}
