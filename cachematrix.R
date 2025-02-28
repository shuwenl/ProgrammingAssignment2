#write a pair of functions that cache the inverse of a matrix


#Write the following functions:
  
#  makeCacheMatrix: This function creates a special "matrix" object that can cache its inverse.

#cacheSolve: This function computes the inverse of the special "matrix" returned by makeCacheMatrix above. 
#If the inverse has already been calculated (and the matrix has not changed), then the cachesolve should 
#retrieve the inverse from the cache.

#Computing the inverse of a square matrix can be done with the solve function in R. 
#For example, if X is a square invertible matrix, then solve(X) returns its inverse.

#For this assignment, assume that the matrix supplied is always invertible.


#learn calculate Inverse using fuctions

makeCacheMatrix <- function(x = matrix()) {
  m <- NULL
  set <- function(y) {
    x <<- y
    m <<- NULL
  }
  get <- function() x
  setinverse <- function(inverse) m <<- inverse
  getinverse <- function() m
  list(set = set, get = get,
       setinverse = setinverse,
       getinverse = getinverse)
}

cacheinverse <- function(x, ...) {
  m <- x$getinverse()
  if(!is.null(m)) {
    message("getting cached data")
    return(m)
  }
  data <- x$get()
  m <- solve(data, ...)
  x$setinverse(m)
  m
}

z <- matrix(c(1,2,3,4),2,2)
M <- makeCacheMatrix(z)
cacheinverse(M)

# create a empty matrix for future comparison and testing
matrix_name <- matrix(1:4, nrow = 2, ncol = 2)
print(matrix_name)
solve(matrix_name)
