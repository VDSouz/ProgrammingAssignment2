## Matrix inversion is usually a costly computation and there may be some benefit to caching the inverse of a matrix rather than compute it repeatedly.
## Computing the inverse of a square matrix can be done with the solve function in R. In this case, assume that the matrix supplied is always invertible.
## The below two functions are used to create a special object that stores matrix object and cache's its inverse. 

## The first function, makeCacheMatrix creates a special "matrix" object, which is really a list containing a function to 
## set the value of the matrix
## get the value of the matrix
## set the value of inverse of the matrix
## get the value of inverse of the matrix

makeCacheMatrix <- function(x = matrix()) {
	inverse <- NULL
    set <- function(Y){
    X <<- Y
    inverse <<- NULL
  }
  get <- function() X
  setinverse <- function(Inverse) inverse <<- Inverse
  getinverse <- function() inverse
  list(set=set,get=get,setinverse=setinverse,getinverse=getinverse)
}


## The second function, cacheSolve calculates the inverse of the special "matrix" object created with the above function.
## However, it first checks to see if the inverse has already been calculated. If so, it gets the inverse from the cache and skips the computation. 
## Otherwise, it calculates the inverse of the data and sets the value of the inverse in the cache via the setinverse function.

cacheSolve <- function(x, ...) {
  inverse <- x$getinverse()
  if(!is.null(inverse)){
    message("getting cached data")
    return(inverse)
  }
  data <- x$get()
  inverse <- solve(data, ...)
  x$setinverse(inverse)
  inverse
}
