## two functions that cache the inverse of the matrix that is included as an argument (x)

## This function creates a special "matrix" object that can cache its inverse

makeCacheMatrix <- function(x = matrix()) {
  
  inv <- NULL
  
  set <- function(matrix) {
    x <<- matrix
    inv <<- NULL
  }
  
  get <- function() x
  setInverse <- function(inverse) inv <<- inverse
  getInverse <- function() inv
  
  ## Return a list of the methods
  
  list(set = set, get = get,
       setInverse = setInverse,
       getInverse = getInverse)
}



## This function computes the inverse of the special "matrix" returned by makeCacheMatrix above. 
## If the inverse has already been calculated (and the matrix has not changed), 
## then the cachesolve should retrieve the inverse from the cache

cacheSolve <- function(x, ...) {
        
    m <- x$getInverse()
    
    if( !is.null(m) ) {
      
      message("getting cached data")
      return(m)
    }
    
    data <- x$get()
    
    m <- solve(data) 
    
    x$setInverse(m)
    
    m
}



