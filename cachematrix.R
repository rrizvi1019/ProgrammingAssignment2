## Put comments here that give an overall description of what your
## functions do

## Write a short comment describing this function
##makeCacheMatrix: This function creates a special "matrix" object that can 
##cache its inverse
##set the value of the matrix
##get the value of the matrix
##setinverse - set the value of the inverse matrix 
##getinverse - get the value of the inverse matrix
makeCacheMatrix <- function(x = matrix()) {

inv_m <- NULL
  set <- function(y) {
    x <<- y
    inv_m <<- NULL
  }
  get <- function() {
    x }
  setinverse <- function(inversematrix) { 
    inv_m <<- inversematrix
  }
  getinverse <- function() {
    inv_m
  }
  list(set = set, get = get,
       setinverse = setinverse,
       getinverse = getinverse)
}

}

## Write a short comment describing this function
##cacheSolve: This function computes the inverse of the special "matrix" 
##returned by makeCacheMatrix above. 
##If the inverse has already been calculated (and the matrix has not changed), 
##then the cachesolve should retrieve the inverse from the cache.

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
        m_inv <- x$getinverse()
## if inverse matrix is already calculated, get the cache data
  if(!is.null(m_inv)) {
    message("getting cached data")
    return(m_inv)
  }
  data <- x$get()
## Calculate inverse of the matrix using solve function
  m_inv <- solve(data, ...)
  x$setinverse(m_inv)
  m_inv
}
