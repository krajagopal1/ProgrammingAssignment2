## First functions creates the matrix with data and functions encapsulated and 
## Second function will create an inverse of the matrix if does not exist
## and cache it in within the encapsulated object.


## This functions creates a special matrix with functions 
## that gets and sets matrix value and inverse matrix value

makeCacheMatrix <- function(x = matrix()) {
  ## initialize inverse matrix var
  invMat <- NULL
  
  ## function to assign matrix value, initializes inverse matrix
  set <- function(y) {
    x <<- y
    invMat <<- NULL
  }
  
  ## function to return matrix value
  get <- function() x
  
  ## function to assign the incoming inverse value
  setinverse <- function(invMatIn) invMat <<- invMatIn
  
  ## function to return the inverse matrix value
  getinverse <- function() invMat
  
  ## returning a list with functions
  list(set = set, get = get,
       setinverse = setinverse,
       getinverse = getinverse)

}

## This function verifies whether inverse already exists for a matrix in the cache
## if not, it creates a cache and stores it using functions & variables exposed by 
## makecachematrix because of lexical scoping

cacheSolve <- function(x, ...) {
  ## returns the inverse of the matrix using getinverse function
  invMat <- x$getinverse()
  
  ## verifies if the inverse exists, if exists, returns cached value
  if(!is.null(invMat)) {
    message("getting cached matrix")
    return(invMat)
  }
  
  ## gets the matrix
  mat <- x$get()
  
  ## creates the inverse of the matrix
  invMat <- solve(mat)
  
  ## stores it inverse variable using setinverse function
  x$setinverse(invMat)
  
  ## returns the inverse
  return(invMat)
}
