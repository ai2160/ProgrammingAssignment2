## Put comments here that give an overall description of what your
## functions do
## The two functions in this file can be used to cache inverse of a matrix

## First step is to create a cached version of the matrix using makeCacheMatrix
## Output from the first version can be passed to the second functioncacheSolve,
## cacheSolve efficiently computes inverse of the original matrix using caching
## i.e., it only computes inverse once each time contents are changed


## Write a short comment describing this function
## This function takes a invertible matrix and returns a cached version of it
## for use with cacheSolve. If returned value is rtv, original matrix can be
## retrived from rtv using rtv$get(), new content can be set using rtv$set()
makeCacheMatrix <- function(x = matrix()) {
  i <- NULL
  set <- function(y) {
    x <<- y
    i <<- NULL
  }
  get <- function() x
  setinverse <- function(inverse) i <<- inverse
  getinverse <- function() i
  list(set = set, get = get,
       setinverse = setinverse,
       getinverse = getinverse)
}


## Write a short comment describing this function
## This function accepts a cached version of matrix and returns its inverse
cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
  i <- x$getinverse()
  if(!is.null(i)) {
    message("getting cached data")
    return(i)
  }
  data <- x$get()
  i <- solve(data)
  x$setinverse(i)
  i
}
