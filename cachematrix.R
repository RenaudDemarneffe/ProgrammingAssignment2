###########################################################################################
##
## The two functions into this script are designed to cache the result of the computation
## of the inverse of a matrix (whidh is time consuming).
## These functions uses scoping rules of R to cache the data.
##
## Remark: These functions assume that the matrix supplied is always invertible.
##
###########################################################################################


## The "makeCacheMatrix" function creates a new object :
## a special "matrix", based on a standard matrix of R,
## which is really a list containing functions to :
##   1. Set the value of the matrix;
##   2. Get the value of the matrix;
##   3. Set the value of the inverse of the matrix;
##   4. Get the value of the inverse of the matrix.

makeCacheMatrix <- function(x = matrix()) {
  # Assign the inverse of the matrix to NULL.
  i <- NULL
  
  # Assign the set. When a new matrix is set up into the "special" matrix,
  # the cached inverse value of the old matrix is resetted.
  set <- function(y) {
    x <<- y
    i <<- NULL
  }
  
  ## Assign the set, setinverse and getinverse functions.
  get <- function() x
  setinverse <- function(inverse) i <<- inverse
  getinverse <- function() i
  
  # Generate the return value. It's a list with the defined functions.
  list(set = set, get = get,
       setinverse = setinverse,
       getinverse = getinverse)
}



## The function "cacheSolve" calculates the inverse of a "special" matrix.
## This "special" matrix is created with the above function (makeCacheMatrix).
## However, it first checks to see if the inverse of the matrix has already
## been calculated. If so, it `get`s the inverse from the
## cache and skips the computation. Otherwise, it calculates the inverse of
## the matrix and sets the value of the inverse in the cache via the `setinverse`
## function.

cacheSolve <- function(x, ...) {
  # Get the cache value for the inverse of the matrix. If this value is not
  # null, return it.
  i <- x$getinverse()
  if(!is.null(i)) {
    message("getting inverse of matrix from cached data")
    return(i)
  }
  
  # The cache value is null. Compute the inverse of the matrix, store the value
  # into the cache and return the value.
  message("compute inverse of matrix and store into cached data")
  data <- x$get()
  i <- solve(data, ...)
  x$setinverse(i)
  i
}
