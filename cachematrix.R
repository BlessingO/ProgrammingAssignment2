## This function cache the inverse of a matrix
## The functions stores a matrix and cache's its inverse

##  This function creates a special "matrix" object that can cache its inverse

makeCacheMatrix <- function(x = matrix()) {
  matInv <- NULL
  set <- function(y) {  #Set the element of the matrix
    x <<- y
    matInv  <<- NULL
  }
  get <- function() x   #Get the element of the matrix
  setInverse <- function(inverse) matInv  <<- inverse
  getInverse <- function() matInv 
  list(set = set, get = get,
       setInverse = setInverse,  #Set the inverse of the matrix
       getInverse = getInverse)   #Get the inverse of the matrix
}


## The function below calculates the inverse of the matrix created with previous function
##If the inverse has already been computes (and the matrix has not been altered), 
##then the cachesolve should retrieve the inverse from the cache.

cacheSolve <- function(x, ...) {
  ## Return a matrix that is the inverse of 'x'
  matInv  <- x$getInverse()
  if(!is.null(matInv )) {
    message("getting cached matrix")
    return(matInv )
  }
  data <- x$get()
  matInv  <- solve(data, ...)
  x$setInverse(matInv )
  matInv 
}

