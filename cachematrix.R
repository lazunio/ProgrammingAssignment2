## The functions below create a special "matrix" object which
## stores a matrix and caches its inverse

# The first function, `makeCacheMatrix` creates a list containing a function to
# 
# 1.  set the value of the matrix
# 2.  get the value of the matrix
# 3.  set the value of the inverse
# 4.  get the value of the inverse

makeCacheMatrix <- function(x = matrix()) {
      i <- NULL
      set <- function(y)
      {
            x <<- y
            i <<- NULL
      }
      get <- function() x
      setInverse <- function(inverse) i <<- inverse
      getInverse <- function() i
      list(set = set, get = get, setInverse = setInverse, getInverse = getInverse)
}

# The function 'cacheSolve' calculates the inverse of the special "matrix"
# created with makeCacheMatrix(). If the inverse has already been calculated
# and cached, the function will 'get' the inverse, skipping the computation.
# Otherwise it will calculate the inverse and cache the value via the 
# 'setInverse' function.

cacheSolve <- function(x, ...) {
      ## Return a matrix that is the inverse of 'x'
      i <- x$getInverse()
      if(!is.null(i))
      {
            message("getting cached data")
            return(i)
      }
      data <- x$get()
      i <- solve(data, ...)
      x$setInverse(i)
      i
}
