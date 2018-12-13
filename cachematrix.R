## Functions to cache the inverse of a matrix

## The makeCachematrix function can be used to cache the
## inverse of the function being looked at. 

makeCacheMatrix <- function(x = matrix()) {
  m <- NULL
  set <- function(y) {
    x <<- y
    m <<- NULL
  }
  get <- function() x
  setcache <- function(inverse) m <<- inverse
  getcache <- function() m
  list(set = set, get = get,
       setcache = setcache,
       getcache = getcache)
}


## The cacheSolve function finds the value of the inverse of
## the matrix from the function above. Part of the function
## will see if the inverse is calculated, if not then the 
## function should find the inverse from the information 
## stored.

cacheSolve <- function(x, ...) {
  m <- x$getcache()
  if(!is.null(m)) {
    message("loading matrix")
    return(m)
  }
  else{
  dataformatrix <- x$get()
  m <- solve(dataformatrix, ...)
  x$setcache(m)
  return(m)
}
}