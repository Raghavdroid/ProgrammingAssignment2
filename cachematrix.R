## Functions that cache the inverse of a matrix


##This is a special matrix that creates a object that can cache its inverse
makeCacheMatrix <- function( x = y() ) {
  
  ## Initialize the inverse as Null
  i <- NULL
  
  ## Matrix set method
  set <- function( y ) {
    x <<- y
    i <<- NULL
  }
  
  ## Matrix get method
  get <- function() {
    ## Return matrix
    x
  }
  
  ## set inverse of the matrix
  setInverse <- function(inverse) {
    i <<- inverse
  }
  
  ## get inverse of the matrix
  getInverse <- function() {
    
    i
  }
  
  ## Return 
  list(set = set, get = get,
       setInverse = setInverse,
       getInverse = getInverse)
}


## Cachesolve method
cacheSolve <- function(x, ...) {
  
  ## the inverse of 'x'
  m <- x$getInverse()
  
  ## Return 
  if( !is.null(m) ) {
    message("getting cached data")
    return(m)
  }
  
  ## matrix
  data <- x$get()
  
  ## Calculating inverse 
  m <- solve(data) %*% data
  
  ## Set Inverset
  x$setInverse(m)
  
  ## Return the matrix
  m
}