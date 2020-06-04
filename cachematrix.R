# This function creates a special "matrix" object
# that can cache its inverse.

makeCacheMatrix <- function(x = matrix()) {
      m <- NULL
      
      set <- function(y) {
            x <<- y
            m <<- NULL
      }
      
      get <- function() x
      
      # solve() is a function in the {base} package that returns
      # the inverse of a matrix. The advantage of solve() is that
      # we do not need to load another package to compute the inverse
      # of a matrix. 
      # The result which solve() gives is very satisfactory.
      setsolve <- function(solve) m <<- solve
      getsolve <- function() m
      
      # Return a list to be called by the cacheSolve() function.
      list(set = set, get = get,
           setsolve = setsolve,
           getsolve = getsolve)
}


# This function computes the inverse of the special "matrix" 
# returned by makeCacheMatrix above. If the inverse has already been 
# calculated (and the matrix has not changed), then the cachesolve 
# should retrieve the inverse from the cache.

cacheSolve <- function(x, ...) {
      ## Return a matrix that is the inverse of 'x'
      m <- x$getsolve()
      
      # Test if the inverse of the matrix is already been cached.
      # If it is, then simply return without painstakingly computing
      # the inverse again.
      if(!is.null(m)) {
            message("getting cached data")
            return(m)
      }
      
      # If not, compute the inverse and return it.
      data <- x$get()
      m <- solve(data, ...)
      x$setsolve(m)
      m
}

# Done!!! Nice and Clear!!!
# I love R!!!