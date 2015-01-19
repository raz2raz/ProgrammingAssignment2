## These two functions, together, allow the user to cache the inverse of a vector.
## USAGE:
## 1. Create a vector and assign it to a variable 
##    Example: x=rbind(c(1, 3,3), c(1,4,3), c(1,3,4))
## 2. Execute MakeCacheMatrix, using above vector, assigning the list output to a variable.
##    Example: mat <- makeCacheMatrix(x)
## 3. Execute CacheSolve, using above list variable. Inverted Matrix is output.
##    Example: CacheSolve(mat)



## MakeCacheMatrix
## OUtputs a list containing the functions and temporary environment pointers to used to cache a matrix of data
## See USAGE above
     
makeCacheMatrix <- function(x = matrix()) {

     m <- NULL
     set <- function(y) {
          x <<- y
          m <<- NULL
     }
     get <- function() x
     setinv <- function(solve) m <<- solve
     getinv <- function() m
     list(set = set, get = get,
          setinv = setinv,
          getinv = getinv)
}

## MakeCacheMatrix
## Inverts a matrix and caches the result for faster subsequent executions
## WHen executed, it checks the existence of an inverted matrix in variable m utilizing the stored function getinv
## If m exists, the results are output
## If m does not exist, the original matrix x is obtained via the stored function get, and an inverted matric is computed and output.
## See USAGE above

cacheSolve <- function(x, ...) {
     m <- x$getinv()
     if(!is.null(m)) {
          message("getting cached data")
          return(m)
     }
     data <- x$get()
     m <- solve(data, ...)
     x$setinv(m)
     m
}
