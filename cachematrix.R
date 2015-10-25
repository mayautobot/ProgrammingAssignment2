## Put comments here that give an overall description of what your
## functions do

## this function returns the list of getter and setter function for invertible matrix and inverse of that invertible matrix

makeCacheMatrix <- function(x = matrix()) {
	##variable holding inverse of the matrix: x
  invM <- NULL
  
  ##setting the invertible matrix x
  setMatrix <- function(y){
    x <<- y
    invM <<- NULL
  }
  
  ##returning the invertible matrix x
  getMatrix <- function(){
    x
  }
  
  ##setting the inverse of the invertible matrix x
  setInverse <- function(inverseM){
    invM <<- inverseM
}


##function for caching inverse of the invertible matrix using solve function if the inverse
##of the matrix is not already set, i.e., inverse of the matrix is null

cacheSolve <- function(x, ...) {
  ## Returns a matrix that is the inverse of 'x'
  
	##getting inverse of the invertible matrix
  invM <- x$getInverse()
  
  ##checking if the got inverse is null or not, if not returns it
  if(!is.null(invM)){
    message("getting cached inverse of the matrix")
    return(invM)
  }
  
  
  sourceMatrix <- x$getMatrix()
  
  ##calculates inverse of the invertible matrix using solve()
  invM <- solve(sourceMatrix, ...)
  
  x$setInverse(invM)
  
  invM
}
