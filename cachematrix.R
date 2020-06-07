## matrix inversion fuction for assignment 2 example for lexical scoping

## makeCacheMatrix fuction is for setting and getting the matrix
## by defining the functions ser and get
## setinvers sets the inverse matrix and getinverse gets the inverse matrix
## list returns the list of set and get functions
## m is the inverse matrix desired

makeCacheMatrix <- function(x = matrix()) {
  m <- NULL
  set <- function(y){
    x <<- y
    m <<- NULL
  }
  get <- function()x
  setinverse <- function(inverse) m <<- inverse
  getinverse <- function() m 
  list(set = set, get = get, 
       setinverse = setinverse, 
       getinverse = getinverse)

}


## cachesolve solves for the inverse of the matrix, if the inverse is already calculated,
## prints a messages and returns the inverse
## or else calculates it

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
  m <- x$getinverse()
  if(!is.null(m)){
    message("getting cached data")
    return(m)
  }
  dat <- x$get()
  m <- solve(dat,...)
  x$setinverse(m)
  m
  
}
