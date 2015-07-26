## makeCacheMatrix function consists of 4 functions including: set, get, setInv, and getInv
## makeCacheMatrix andcacheSolve functions calculate a matrix and its inverse, 
## and store them in and recall them from cache

makeCacheMatrix <- function(x = matrix()) {
  
  # Inv_Of_x is a variabe for storing the inverse of x
  Inv_Of_x <- NULL 
  
  
  # set function sets a matrix to object created by makeCacheMatrix function
  set <- function(y) {
    x <<- y
    
    # Inv_Of_x  is a variabe for storing the inverse of x, the initial value of which is set to Null
    Inv_Of_x <<- NULL 
  }
  
  
  get <- function() x # it returns x
  setInv <- function(inv) Inv_Of_x <<- inv # set the inverse of the matrix
  getInv <- function() Inv_Of_x # return the inversed matrix
  
  # then list returns a listconsisting of the abovementioned functions
  list(set = set, get = get, setInv = setInv, getInv = getInv)
  
}






cacheSolve <- function(x, ...) {
  
  # get the value of the inverse of 'x'
  INV_X <- x$getInv() 
  
  
  # We check if the this inverse can be calculated by "if" 
  if(!is.null(INV_X )) { 
    message(" getting cached data ")
    
    ## Return a matrix that is the inverse of 'x'
    return(INV_X ) 
  }
  
  
  # else, we use x$get to get the matrix object
  data <- x$get() 
  
  # solve it for obtaining the inverse value
  INV_X  <- solve(data) 
  x$setInv(INV_X ) 
  
  ## Return a matrix that is the inverse of 'x'
  INV_X  
  
  
}
