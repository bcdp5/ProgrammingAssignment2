## Put comments here that give an overall description of what your
## functions do

## Define 'makeCacheMatrix' to cache a matrix and its inverse.
## It includes 4 sub-functions:
##        1.  'get' get the value of the vector
##        2.  'set' it sets the value of its argument to the cached matrix
##        3.  'setinv' it sets the value of its argument as the inverse of the cached matrix
##        4.  'getinv' it gets the value of the inverse of the cached matrix
## The function returns a list containing the functions listed above
makeCacheMatrix <- function (x = matrix()){
          inv <- NULL
          
          set <- function (y){
            x <<- y
            inv <<- NULL
          }
          
          get <- function() x
          
          setinv <- function(inverse) inv <<- inverse
          
          getinv <- function() inv
          
          list(set = set, get = get,
               setinv = setinv,
               getinv = getinv)
}



## Define 'cacheSolve' to computes the inverse of the cached matrix from 'makeCacheMatrix'.

cacheSolve <- function (x,...){
          # check if the cached matrix has a previously defined inverse, if yes it returns that.
          inv <- x$getinv()
          if (!is.null(inv)){
            message("getting cached data")
            return(inv)
          }
          
          # Compute the inverse using basi solve(), then set this inverse to the cached matrix
          # and finally returns it.
          data <- x$get()
          inv <- solve(data,...)
          x$setinv(inv)
          inv
}
