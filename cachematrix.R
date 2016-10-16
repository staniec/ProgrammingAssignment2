## The functions below extend build-in matrix object with cashed inverse to demonstrate scopes in R. 
## Warning: we assume matrix is invertable and ignore b parameter of solve!
## Example:
# m <- makeCacheMatrix(matrix(data = c(1,2,3,4), nrow = 2, ncol = 2))  # creates cached matrix
# n <- cacheSolve(m)                                                   # first call - slow, calculates the inverse 
# m$get() %*% n                                                        # use get() to access the value of cashedMatrix 
# n <- cacheSolve(m)                                                   # second call - fast, uses cached result
# m$set(n)                                                             # use set() to change the value of cashedMatrix
#                                                                      # invalidates the cache
# n <- cacheSolve(m)                                                   # slow, calculates the inverse again


## This is a constructor / factory function which converts matrix into cashed matrix.
## The matrix data should be accessed and changed by get and set methods respectivelly.

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



## This is an extension of the base solve function for cachedMatrix

cacheSolve <- function(x, ...) {
    ## Return a matrix that is the inverse of 'x'
    i <- x$getinverse()
    if(!is.null(i)) {
      message("getting cached data")
      return(i)
    }
    data <- x$get()
    i <- solve(data, ...)
    x$setinverse(i)
    i
}
