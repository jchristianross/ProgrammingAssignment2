## These functions are used to calculate and cache a matrix and its inverse

## Creates a list of get/set inverse functions for a matrix; feeds into cacheSolve
makeCacheMatrix <- function(x = matrix()) {
     s <- NULL
     set <- function(y) {
          x <<- y
          s <<- NULL
     }
     get <- function() x
     setsolve <- function(solve) s <<- solve
     getsolve <- function() s
     list(set = set, get = get, setsolve = setsolve, getsolve = getsolve)
}

## Returns the cached inverse if available, otherwise calculates and caches it
cacheSolve <- function(x, ...) {
     s <- x$getsolve()
     if(!is.null(s)) {
          message("getting cached data")
          return(s)
     }
     data <- x$get()
     s <- solve(data, ...)
     x$setsolve(s)
     s
}

## Example operations to test the basics of the functions above
m1 <- matrix(1:4, nrow=2, ncol=2)
m2 <- matrix(c(1,0,5,2,1,6,3,4,0), nrow=3, ncol=3)

cm1 <- makeCacheMatrix(m1)
cm2 <- makeCacheMatrix(m2)
cacheSolve(cm1)
m3 <- cacheSolve(cm2)
m3
cacheSolve(cm1)
cacheSolve(m3)

cm3 <- makeCacheMatrix(m3)
cacheSolve(cm3)
cacheSolve(cm3)
cacheSolve(cm2)

## Also testing for matrices that have changed after being cached
cm1$set(matrix(c(2,2,3,2), nrow=2, ncol=2))
cm1$getsolve() ## Will return NULL because the inverse needs to be recalculated
cacheSolve(cm1)
cacheSolve(cm1)
