# This script has two functions used to cache the inverse of a matrix 

# makeCacheMatrix creates a list containing below functions
# 1.set the value of the matrix
# 2.get the value of the matrix
# 3.set the value of the inverse matrix
# 4.get the value of the inverse matrix

makeCacheMatrix <- function(x = matrix()) {
  i <- matrix()         # initialize variable i (used to hold inverse of given matrix) to empty matrix
  set <- function(y) {  # set matrix x to y and set i to empty matrix
    x <<- y
    i <<- matrix()
  }
  get <- function() x  # returns matrix x
  setinverse <- function(InverseMatrix) i <<- InverseMatrix  # sets matrix i in makeCacheMatrix  function 
  getinverse <- function() i   # returns matrix i from makeCacheMatrix funciton
  list(set = set, get = get,   # returns a list of functions set, get, setinverse and getinverse   
       setinverse = setinverse,
       getinverse = getinverse)
}


# The following function calculates the inverse of the matrix created with above makeCacheMatrix function.It first checks to see if the inverse has already been computed.
#If so, it gets the inverse from the cache and skips the computation. Otherwise, it computes the inverse of the given matrix and sets the value of the InverseMatrix (variable i) in the cache using the setinverse function.


cacheSolve <- function(x, ...) {
  i <- x$getinverse()   #attempts to get the inverse matrix for x. This returns empty matrix if inverse is not calculated previously
  
  # If i is not empty matrix that means inverse was already computed so return cached value
  if(!all(is.na(i))) {  
    message("getting cached data")
    return(i)
  }
  # If i is empty then compute inverse and set i
  mat<- x$get()          # set mat to x from makeCacheMatrix 
  i <- solve(mat, ...)   # compute the inverse of matrix using solve function
  x$setinverse(i)        # set the inverse of matrix x to the above calculated inverse
  i
}