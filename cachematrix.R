## This script describes two functions which demonstrate caching capabilities in R to help
## optimize processing by leveraging cache for base information that is used repeatedly as is 
## demonstrated in calculating the inverse of a matrix which can be a very costly operation.
## In this exercise we will create two functions used to cache the inverse of a matrix.

## Function makeCacheMatrix:This function creates a special "matrix" object that can cache its inverse.

makeCacheMatrix <- function(x = matrix()) 
  {
    inv <- NULL
    # 1. set the value of the matrix
    set <- function(y) 
      {
        x <<- y
        inv <<- NULL
      }
    # 2. get the value of the matrix
    get <- function() x
    # 3. set the value of inverse of the matrix
    setinverse <- function(inverse) inv <<- inverse
    # 4. get the value of inverse of the matrix
    getinverse <- function() inv
    list(set=set, get=get, setinverse=setinverse, getinverse=getinverse)
  }

## Function cacheSolve:This function computes the inverse of the special "matrix" returned by makeCacheMatrix above. 
## If the inverse has already been calculated (and the matrix has not changed),
## then the cachesolve should retrieve the inverse from the cache.

cacheSolve <- function(x, ...) 
  {
    inv <- x$getinverse()
    if(!is.null(inv)) 
      {
        message("getting cached data.")
        return(inv)
      }

    data <- x$get()
    inv <- solve(data)
    x$setinverse(inv)
    inv
  }
