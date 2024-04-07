## A pair of functions that cache the inverse of a matrix 

## This function creates a special matrix object that can cache its inverse 

makeCacheMatrix <- function(m = matrix()) {
  
  ## Initialize the inverse property 
  inv <- NULL 
  
  ## Method to set the matrix 
  set <- function(matrix){ 
    m <<- matrix  
    inv <<- NULL 
    
  }
  
  ##Method to get the matrix 
  get <- function(){ 
    ## Return the matrix 
    m 
  }
  
  ## Method to set the inverse of the matrix 
  setInverse <- function(inverse){ 
    inv <<- inverse 
  }
  
  ##Method to get the inverse of the matrix 
  getInverse <- function(){
    inv
  }
  
  ## Return a list of the methods 
  list(set = set, get = get,
       setInverse = setInverse,
       getInverse = getInverse)
  
}

## Compute the inverse of the special matrix returned by "makeCacheMatrix" above 
## If the inverse has already been calculated (and the matrix has not changed), the the "cachesolve" fuction should
## retrieve the inverse from the cache 

cacheSolve <- function(x, ...) {
  ## Return a matrix that is the inverse of 'x'
  i <- x$getInverse()
  
  ## Just return the inverse if it has already been set 
  if (!is.null(i)){
    message("Getting cached data")
    return(inv)
  }
  
  m <- x$get()
  i <- solve(m,...)
  ## Set the inverse to the object 
  x$setInverse(i)
  
  ## Return the matrix 
  i
}