## Put comments here that give an overall description of what your
## functions do

## This function creates a special "matrix" object that can cache its inverse.

makeCacheMatrix <- function(x = matrix()) {
 y <- NULL
 set <- function(z) {
   x <<- z
   y <<- NULL
 }
 get <- function() x
 setsolve <- function(solve) y <<- solve
 getsolve <- function() y
 list (set=set,get=get,setsolve=setsolve,getsolve=getsolve)
}


##This function computes the inverse of the special "matrix" returned 
##by makeCacheMatrix above. If the inverse has already been calculated
##(and the matrix has not changed), then cacheSolve should retrieve 
##the inverse from the cache.

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
  y <- x$getsolve()
  if(!is.null(y)){
    message("get cached data")
    return(y)
  }
  data <- x$get()
  y <- solve(data,...)
  x$setsolve(y)
  y
}

#m <- makeCacheMatrix(matrix(c(1,2,3,4),nrow=2,ncol=2))
#cacheSolve(m)
