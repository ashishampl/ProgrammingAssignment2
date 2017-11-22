## This program contains two functions which caches a costly operation - Inverse of a Matrix
## It caches the inverse of matrix so that it can be read from cache when needed rather than recalculating 


## This function creates a special vector which is a list containing function for
## 1. set: Sets the actual matrix in cache using << operator
## 2. setInverse: Sets the Inverse of actual matrix in cache using << operator
## 3. get: returns the actual matrix
## 4. getInverse: returns the inverse of actual matrix 

makeCacheMatrix <- function(x = matrix()) {
  
  cachedInverseMatrix <- NULL
  
  set <- function(actualMatrix){
    x <<- actualMatrix
    cachedInverseMatrix <<- NULL
  }
  
  setInverse <- function(inversedMatrix){
    cachedInverseMatrix <<- inversedMatrix
  }
  
  get <- function() x
  getInverse <- function() cachedInverseMatrix     
  list(set=set, get=get, setInverse=setInverse, getInverse=getInverse)
}


## This function calls getInverse to get the Inverse of a matrix from cache
## If this is available in cache, then this function returns the cached result
## If this is NOT available in cache, then cached matrix is read by calling get function
## Using solve function, Inverse of this matrix is calculated and returned
## The inverse is then set in cache by calling setInverse function

cacheSolve <- function(x, ...) {
  InverseFromCache <- x$getInverse()
  if(!is.null(InverseFromCache)) {
    message("getting cached data.")
    return(InverseFromCache)
  }
  
  data <- x$get()
  calculatedInverse <- solve(data)
  x$setinverse(calculatedInverse)
  calculatedInverse
}

## VERIFICATION / UNIT TEST
## m <- matrix(c(1,2,3,4),2,2)
## cm <- makeCacheMatrix(m)
## cm$get() 
## cacheSolve(cm)  ## Nothing in cache as of now so calculatiing Inverse. However saves Inverse in cache now
## cacheSolve(cm)  ## This time inverse available in cache. So reading from there
