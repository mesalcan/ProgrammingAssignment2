## Here, we are going to create a special matrix object and then we could get
## its inverse. We are applying the <<- operator which allows us to assign
## a value to an object in an environment that is different from the current
## environment

## makeCacheMatrix is a function that creates a special matrix object
## that can cache its inverse

makeCacheMatrix <- function(x = matrix()) {
  m <- NULL
  set <- function(y) 
  {
    x <<- y
    m <<- NULL
  }
  get <- function() x
  setinverse <- function(solve) m <<- inverse
  getinverse <- function() m
  list(set = set, get = get,
       setinverse = setinverse,
       getinverse = getinverse)
}


## cacheSolve is a function that computes the inverse of a matrix
## by using the "solve" function. It evaluates if the inverse has 
## already been calculated, if so, it returns the value from the cache
## if not, it calculates the inverse from the provided data.

cacheSolve <- function(x, ...) 
{
  ## Return a matrix that is the inverse of 'x'
  m <- x$getinverse()
  if(!is.null(m))
  {
    message("getting cached data")
    return(m)
  }
  data <- x$get()
  m <- solve(data, ...)
  x$setinverse(m)
  m
}
