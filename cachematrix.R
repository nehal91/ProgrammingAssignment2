## The following two functions are used to cache the inverse of a matrix.

## makeCacheMatrix creates a list containing a function to
# 1. set the value of the matrix
# 2. get the value of the matrix
# 3. set the value of inverse of the matrix
# 4. get the value of inverse of the matrix
# as asked in the assignment question.

makeCacheMatrix <- function(x = matrix()) {
         m <- NULL
      set <- function(y) {
            x <<- y
            m <<- NULL
      }
      get <- function() x
      setmatrixinverse <- function(solve) m <<- solve
      getmatrixinverse <- function() m
      list(set = set, get = get,
           setmatrixinverse = setmatrixinverse,
           getmatrixinverse = getmatrixinverse)

}


## The following function returns the inverse of the matrix. 
#  For the first time it computes the inverse, sets the value in the cache via
# setinverse function and from second time, it gets the result and skips the
# computation 

# This function assumes that the matrix is always invertible.

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
        m <- x$getmatrixinverse()
      if(!is.null(m)) {
            message("getting cached data without computation")
            return(m)
      }
      data <- x$get()
      m <- solve(data, ...)
      x$setmatrixinverse(m)
      m
}

## Sample run:
## > x = rbind(c(5, 0), c(0, 5))
## > m = makeCacheMatrix(x)
## > m$get()
##      [,1] [,2]
## [1,]    5    0
## [2,]    0    5

## No cache in the first run
## > cacheSolve(m)
##       [,1] [,2]
## [1,]  0.2  0.0
## [2,]  0.0  0.2

## Retrieving from the cache in the second run
## > cacheSolve(m)
## getting cached data without computation
##       [,1] [,2]
## [1,]  0.2  0.0
## [2,]  0.0  0.2
#  > 
