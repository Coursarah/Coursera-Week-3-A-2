## Put comments here that give an overall description of what your
## functions do
## The purpose of these two functions is to perform generate an inverse matrix. 


## Write a short comment describing this function
##The function makeCacheMatrix creates a matrix from a vector of proposed data.From this the matrix inverse can be cached.

makeCacheMatrix <- function(x = matrix()) {
  inv = NULL 
  set = function(y) {
    x <<- y 
    inv <<- NULL
  }
  get = function() x
  setinv = function(inverse) inv <<- inverse
  getinv = function() inv
  list(set=set, get=get, setinv=setinv, getinv=getinv)
}

## Write a short comment describing this function
## The second function cacheSolve is to take the matrix generated in makeCacheMatrix and to generate the inverse matrix. 

cacheSolve <- function(x, ...) {
  ## Return a matrix that is the inverse of 'x'
  inv=x$getinv()
  mat.data = x$get()
  inv = solve(mat.data,...)
  x$setinv(inv)
  
  return(inv)
}