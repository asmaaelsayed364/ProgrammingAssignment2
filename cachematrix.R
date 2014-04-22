setwd('C:\Users\Asmaa Hassan\Desktop\R\ProgrammingAssignment2-master')
## write a pair of functions, namely,"makeCacheMatrix" and "cacheSolve" 
## both functions will be used to cache the inverse of a matrix

## "makeCacheMatrix" is a function which creates a special "matrix" object 
## this function can cache its inverse for the input (which is an invertible square matrix)

makeCacheMatrix <- function(x = matrix(sample(1:100,9),3,3)) {
  
  s <- NULL
  set <- function(z) {
    x <<- z
    s <<- NULL
  }
  get <- function() x
  setsolve <- function(solve) s <<- solve
  getsolve <- function() s
  list(set = set, get = get,
       setsolve = setsolve,
       getsolve = getsolve)
}

## "cacheSolve" is  a function which computes the inverse of the special "matrix"
## returned by "makeCacheMatrix " above. 
## If the inverse has already been calculated, the " cachesolve " should retrieve the
## inverse from the cache

cacheSolve <- function(x, ...) {
  s <- x$getsolve()
  if(!is.null(s)) {
    message("getting inversed matrix")
    return(s)
  }
  data <- x$get()
  s <- solve(data, ...)
  x$setsolve(s)
  s
}