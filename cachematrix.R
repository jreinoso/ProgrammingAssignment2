# Functin makeCacheMatrix
# input:  a matrix x
# output:  a list with four elements
#               1. function to set the matrix x and clear 
#                         the inverse matrix
#               2.  function to get the original matrix x
#               3.  functin to set the inverse matrix
#               4.  function to get the inverse matrix

makeCacheMatrix <- function(x = matrix(numeric(),0,0)) {
  m <- NULL
  set <- function(y) {
    x <<- y
    m <<- NULL
  }
  get <- function() x
  setm <- function(mean) m <<- mean
  getm <- function() m
  list(set = set, get = get,
       setm = setm,
       getm = getm)
}


# function: cacheSolve
# input: a makeCacheMatrix x
# output:  the inverse of the x$get() matrix if it 
#          can get it from memory or the solve(x$get())
#          which return the calculated inverse matrix and 
#          save it in the makeCacheMatrix for future use.

cacheSolve <- function(x, ...) {
  m <- x$getm()
  if(!is.null(m)) {
    message("getting cached data")
    return(m)
  }
  data <- x$get()
  m <- solve(data)
  x$setm(m)
  m
}
