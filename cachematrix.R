###############################################################################
##                                                                            #
##          Week 3 - Coursera - Data Scientist - R Programming                #
##              Programming Assignment 2: Lexical Scoping                     #
##                                                                            #
###############################################################################


## The below functions can be used to create an object that can hold both a
## matrix and it's inverse.
## The functions are designed in a time efficient manner.

## This function creates a special "matrix" object that can cache its inverse.

makeCacheMatrix <- function(x = matrix()) {
  inversmatrix <- NULL
  set <- function(y) {
    x <<- y
    inversmatrix <<- NULL
  }
  get <- function() x
  setinvers <- function(invmat) inversmatrix <<- invmat
  getinvers <- function() inversmatrix
  list(set = set, get = get,
       setinvers = setinvers,
       getinvers = getinvers)

} # end function

## This function computes the inverse of the special "matrix" returned 
## by makeCacheMatrix above. If the inverse has already been calculated 
## (and the matrix has not changed), then the cachesolve should retrieve 
## the inverse from the cache.

cacheSolve <- function(x, ...) {
        
  inversmatrix <- x$getinvers()
  if(!is.null(inversmatrix)) {
    message("getting cached data")
    return(inversmatrix)
  }
  data <- x$get()
  inversmatrix <- solve(data, ...)
  x$setinvers(inversmatrix)
  inversmatrix

} # end function
