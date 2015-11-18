## Put comments here that give an overall description of what your
## functions do

## Write a short comment describing this function
## Create a function, with 4 functions to set the value of a matrix, get the value of a matrix
## set the solution (inverse matrix) and get the solution.
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


## Write a short comment describing this function
## this function takes a matrix created using makeCacheMatrix and solve the matrix and store the value 
## in a cache variable (s). If s is not empty (i.e. the solution exists already), the cached value will be used, avoiding
## computation. Additionaly a message is printed: "Using cache solution"

cacheSolve <- function(x = matrix(x), ...) {
        ## Return a matrix that is the inverse of 'x'
      s<-x$getsolve()
      if(!is.null(s)){
        message("Using cache solution")
        return(s)
      }
      matrix<-x$get()
      s<-solve(matrix, ...)
      x$setsolve(s)
      s
}
