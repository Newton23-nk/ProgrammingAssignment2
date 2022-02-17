## Put comments here that give an overall description of what your
## functions do

## Write a short comment describing this function

makeCacheMatrix <- function(x = matrix()) {
  m <- NULL
  set <- function(y) {
    x <<- y
    m <<- NULL
    set <- function(y) {            ## to reset matrix 
      x <<- y                     ## reassign "new" matrix to x 
      m <<- NULL                  ## reinitialize m to NULL
    }
    get <- function() x
    setInvmatrix <- function(InvMatrix) m <<- InvMatrix
  }

}


## Write a short comment describing this function
## return an inverse matrix of 'x'

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
  m <- x$getInvmatrix()
  if(!is.null(m)) {
    message("getting cached data")
    return(m)
    
    m <- x$getInvmatrix()              
    if(!is.null(m)) {           ## if same matrix was calculated before
      message("getting cached data")  
      return(m)               ## return old resultant(m)
    }
    data <- x$get()
    m <- solve(data, ...)
    x$setInvmatrix(m)
    m
    data <- x$get()             ## get the uncalculated matrix if otherwise
    m <- solve(data, ...)       ## Inverse matrix
    x$setInvmatrix(m)           ## reassign the inverse matrix
    m                           ## print the inverse matrix
  }
}
