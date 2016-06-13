## Put comments here that give an overall description of what your
## functions do

## Write a short comment describing this function
## This function creates a special "matrix" object that can cache its inverse
## the function generates 4 functions to get or set, getinverse or setinverse of 
## the matrix
makeCacheMatrix <- function(x = matrix()) {
      inver <- NULL
      set <- function(y){
        x <<- y
      inver <<-NULL
      }
      get <- function()x
      setinverse <- function(inverse) inver <<- inverse
      getinverse <- function() inver
      list(set = set, get = get, setinverse = setinverse, getinverse = getinverse)
}


## Write a short comment describing this function
## this function computes the inverse of the special "matix" returned by 
## makeCacheMatrix. if the inverse has already been calculated, then the 
## cachesolve should retrive the inverse from cache

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
        invers <- x$getinverse()
        if(!is.null(invers)){
          message("getting cached data")
          return(inver)
        }
        data <- x$get()
        invers <- solve(data)
        x$setinverse(invers)
        invers
}
