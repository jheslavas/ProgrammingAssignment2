## Functions "makeCacheMatrix" and "cacheSolve"

## These functions, together, cache the inverse of a matrix, to avoid compute it repeteadly,
## given that matrix inversion is usually costly computation. 


## Short comment describing the function: "makeCacheMatrix"

## This function creates a special "matrix" that can cache its inverse. Its ouput should be
## used with the complementary function "cacheSolve" that runs with this output
## Example: i1 <- makeCacheMatrix(x)
##			i2 <- cacheSolve(i1)
## 			i2 ## this will print the inverse of the matrix
##	Tip: The mutiplication of both matrices (original and its inverse) should generate an
## identity matrix with the command : x %*% i2. 

## The "makeCacheMatrix Function"

makeCacheMatrix <- function(x = matrix()) {
  i <- NULL
  set <- function(y = matrix()) {
    x <<- y
    i <<- NULL
  }
  get <- function() x
  setinverse <- function(solve) i <<- solve
  getinverse <- function() i
  list(set = set, get = get,
       setinverse = setinverse,
       getinverse = getinverse)
}
}


## Short comment describing the function: "cacheSolve"

## This function complements the function makeCacheMatrix, and it computes the inverse of the special matrix returned by the function "makeCacheMatrix"

cacheSolve <- function(x, ...) {
  i <- x$getinverse()
  if(!is.null(i)) {
    message("getting cached data")
    return(i)
  }
  data <- x$get()
  i <- solve(data, ...)
  x$setinverse(i)
  return(i)
}	
        ## Return a matrix that is the inverse of 'x'
