## Put comments here that give an overall description of what your
## functions do


# Thefollowing two functions help in calculating and caching the 
#inverse of a matrix.


## Write a short comment describing this function
# makeCacheMatrix creates a list containing a function to
# 1. set the value of the matrix
# 2. get the value of the matrix
# 3. set the value of inverse of the matrix
# 4. get the value of inverse of the matrix

makeCacheMatrix <- function(x = matrix()) {
  resultInverse <- NULL
  set <- function(y) {
    x <<- y
    resultInverse <<- NULL
  }
  get <- function() x
  setinverse <- function(inverse) resultInverse <<- inverse
  getinverse <- function() resultInverse
  list(set=set, get=get, setinverse=setinverse, getinverse=getinverse)
}


## Write a short comment describing this function
# The following function returns the inverse of the matrix. 
# This function checks if inverse is available in cache.  
# If not, this function caluclates and sets it in cache.

cacheSolve <- function(x, ...) {
  ## Return a matrix that is the inverse of 'x'
  cachedInverse <- x$getinverse()
  if(!is.null(cachedInverse)) {
    message("getting cached Inverse...")
    return(cachedInverse)
  }
  data <- x$get()
  
  #calculating inverse and setting it into cache
  message("Calculating Inverse...")
  cachedInverse <- solve(data)
  x$setinverse(cachedInverse)
  cachedInverse
  
}

#Test executions

#> x<-cbind(c(-3,8), c(1,-1))
#> x
#     [,1] [,2]
#[1,]   -3    1
#[2,]    8   -1
#> m = makeCacheMatrix(x)

#> cacheSolve(m)
#Calculating Inverse...
#     [,1] [,2]
#[1,]  0.2  0.2
#[2,]  1.6  0.6

#> cacheSolve(m)
#getting cached data.
#     [,1] [,2]
#[1,]  0.2  0.2
#[2,]  1.6  0.6