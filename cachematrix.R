## Matrix inversion is usually a costly computation and there may 
## be some benefit to caching the inverse of a matrix rather than 
## computing it repeatedly. 
##
## Here we have a pair of functions, the first to create a special "matrix"
## and the second to solve the inverse of the matrix. However if the inverse
## of the matrix has already been solved and the matrix hasn't changed 
## then the second function will take the solution from the cache.
##
## The first function, makeCacheMatrix creates a special "matrix", 
## which is really a list containing a function to
## 1. set the value of the matrix
## 2. get the value of the matrix
## 3. set the value of the inverse matrix
## 4. get the value of the inverse matrix

makeCacheMatrix <- function(x = matrix()) {
  m <- NULL
  set <- function(y) {
    x <<- y
    m <<- NULL
  }
  get <- function() x
  setmat <- function(solve) m <<- solve
  getmat <- function() m
  list(set = set, get = get, setmat = setmat, getmat = getmat)
}


## now solve the inverse of the matrix using the cache if set

cacheSolve <- function(x = matrix(), ...) {
        ## Return a matrix that is the inverse of 'inputMatrix'
  m <- x$getmat()
  if(!is.null(m) & identical(x,m)) {
    message("getting cached data")
    return(m)
  }
  data <- x$get()
  m <- mean(data, ...)
  x$setmat(m)
  m
}
