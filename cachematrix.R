## Create a matrix object and Cache the solution to the inverse of the Matrix


## makeCacheMatrix creates a special Matrix object that can cache its inverse.

makeCacheMatrix <- function(x = matrix()) {
  m <- NULL     #initial state of m is null
  set <- function(y) { # internal function to set value of x
    x <<- y
    m <<- NULL
  }
  get <- function() {  #function to retrieve the value of x
    return (x)
  }
    
  setinverse <- function(inverse) {  # Function to setinverse 
    m <<- inverse          
  }
    getinverse <- function() {  #function to getinverse of matrix
      return(m)
}
  list(set = set, get = get,    #member list
       setinverse = setinverse,
       getinverse = getinverse)
}


## cacheSolve computes the inverse of a special matrix returned by makeCacheMatrix
## If the inverse has already been calculated (and the matrix has not changed), 
##then cacheSolve should retrieve the inverse from the Cache.

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
  m <- x$getinverse()     # set m to cached inverse
  if(!is.null(m)) {       # return cached value if not null
    message("getting cached data")
    return(m)
  }
  data <- x$get()         
  m <- solve(data, ...)   # solve for inverse of x
  x$setinverse(m)         #cache inverse by setinverse function
  m
}
