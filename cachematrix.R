
# get : gets the matrix
#set: sets matrix
#getinv: gets the inverse of the matrix
#setinv : sets the inverse of the matrix

## this function creates a special "matrix" object, 
## which is really a list containing functions 
#get, set, getinv, setinv

makeCacheMatrix <- function(x = matrix()) {
  inv <- NULL
  
  #Store a matrix
  set <- function(y)
  {
    x <<- y
    inv <<- NULL
  }
  
  #returns a stored matrix
  get <- function() x
  
  #cache the given argument
  setinv <- function(i){
    inv <<- i
  }
  
  #get the cached value
  getinv <- function() inv
  
  #return a list, each named element is a function
  list(set=set, get=get, setinv = setinv, getinv = getinv)
  
}


##Following function calculates the inverse of a "special" matrix
##created ewith makeCacheMatrix

cacheSolve <- function(x, ...) {
  ## Return a matrix that is the inverse of 'x'
  i <- x$getinv()
  
  # if a cached value exists, return it.
  if(!is.null(i)){
    message("getting cached data")
    return(inv)
  }
  
  #else get the matrix, calculate the inverse, store it in cache
  mat <- x$get()
  i <- solve(mat,...)
  x$setinv(i)
  return(i)
}
