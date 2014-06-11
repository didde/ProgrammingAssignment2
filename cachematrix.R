## OVERALL DESCRIPTION ##
## by: D Ekeblom.
##
## The function makeCacheMatrix(x) takes a positive semidefinite matrix (i.e. invertible)
## and turns it into a list Z that mimics an object with four methods (set(), get(), setm(), 
## getm()) that allows the inverse of the matrix x to be stored and retrieved.
## 
## The function cacheSolve(Z) retrieves the matrix x from the list Z and returns
## the inverse of x, in case it has not been computed before. Upon computing the inverse of x,
## it is stored in the list Z. If the inverse of x has been computed and stored in list Z, 
## the function cacheSolve(Z) returns  the stored inverse instead of computing it.
##
## The code is commented below in greater detail.

makeCacheMatrix <- function(x = matrix()) {
  
  m <- NULL
  
  # The matrix x stored in cache can be overridden with the function set().
  set <- function(y) {
    x <<- y
    m <<- NULL
  }
  
  # The function get() retrieves the matrix x from the cache.
  get <- function() x
  
  # The function setm() uses the operator '<<-' to allow the parameter 'invMatrix' 
  # to pass an argument from the calling environment, outside of the local scope.
  # I.e. if the calling environment is the function cacheSolve(), then 
  # 'invMatrix' will pass the inverse (see line 65 below) as an argument 
  # to be stored in cache.
  setm <- function(invMatrix) m <<- invMatrix
  
  # getm() gets the inverse from the cache, if it has been stored using setm(); 
  # if not, it returns NULL (see line 16 or 21)
  getm <- function() m
  
  # The functions set(), get(), setm(), getm() are returned in a list. 
  # The variables stored in cache are not returned explicitly.
  list(set=set, get = get, setm = setm, getm = getm)
}


cacheSolve <- function(x, ...) {
  
  # Gets the inverse of matrix x from list Z, stored in cache
  # (NB: list Z is passed via parameter x as an argument)
  m <- x$getm()
  
  # Checks if an inverse of matrix x was returned from the list Z.
  # If an inverse has been stored, it is returned and function exits.
  if(!is.null(m)) {
    message("Getting cached inverse")
    return(m)
  }
  
  # If inverse of x has not been stored, then matrix x is retrieved.
  data <- x$get()
  
  # Inverse is computed
  m <- solve(data, ...)
  
  # Invers is stored in list Z.
  x$setm(m)
  
  # Inverse is returned
  m
}
