## This file contains two functions, the makeCacheMatrix and the cacheSolve 
## The makeCacheMatrix creates a list with four functions used to retreive and set the matrix passed as an argument as well as his inverse. 
## The cacheSolve calculates the matrix's inverse if it doesn't exist in the cache


## Creates a list to of functions used to retreive and set the matrix passed as an argument as well as his inverse 
makeCacheMatrix <- function(x = matrix()) {
    inverseMatrix <- NULL
    set <- function(y) {
        x <<- y
        inverseMatrix <<- NULL
    }
    get <- function() x
    setInverseMatrix <- function(inverseMatrixArg) inverseMatrix <<- inverseMatrixArg
    getInverseMatrix <- function() inverseMatrix
    list(set = set, get = get,
         setInverseMatrix = setInverseMatrix,
         getInverseMatrix = getInverseMatrix)
}


## Calculates and returns the inverse of the matrix passed as an argument if it has not been calculated and cached before

cacheSolve <- function(x, ...) {
  cachedInverseMatrix <- x$getInverseMatrix()    
  
  inverseMatrix <- if(!is.null(cachedInverseMatrix)) {
      message("Getting cached inverse matrix")
      cachedInverseMatrix
  } else {
      message("Calculating inverse matrix and cache it")
      data <- x$get()   
      calculatedInverseMatrix <- solve(data)
      x$setInverseMatrix(calculatedInverseMatrix)
      calculatedInverseMatrix
  }
  
  inverseMatrix
}