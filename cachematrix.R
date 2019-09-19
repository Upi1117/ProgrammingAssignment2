## Our aim is to write a pair of functions, namely, 
## "makeCacheMatrix" and "cacheSolve" that cache the inverse of a matrix

## makeCacheMatrix is a function which creates a special "matrix" object that can 
## cache its inverse for the input (which is an invertible square matrix)


makeCacheMatrix <- function(x = matrix()) {
  inv <- NULL
  
  ## set the value of the matrix
  set <- function(y) {
    x <<- y
    inv <<- NULL
  }
  
  ## get the value of the matrix
  get <- function() x
  
  ## set the value of the inverse
  setinverse <- function(inverse) inv <<- inverse
  
  ## get the value of the inverse
  getinverse <- function() inv
  list(set = set, get = get,
       setinverse = setinverse,
       getinverse = getinverse)
  
}


## This second function computes the inverse of the special "matrix" created by 
## makeCacheMatrix above. If the inverse has already been calculated (and the 
## matrix has not changed), then it should retrieve the inverse from the cache.

cacheSolve <- function(x, ...) {
  ## Return a matrix that is the inverse of 'x'
  
  
  
  ##first checks to see if the inverse has already been calculated.If so, 
  ##gets the inverse from the cache and skips the computation.
  inv <- x$getinverse()
  if(!is.null(inv)) {
    message("getting cached data")
    return(inv)
  }
  
  ##gets the data  
  data <- x$get()
  
  ##calculates the inverse of the data
  inv <- solve(data, ...)
  
  ##sets the value of the inverse in the cache via the setinverse function.
  x$setinverse(inv)
  inv
}

