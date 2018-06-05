# Define Getter Setter for Cached Matrix and Matrix Inverse 
makeCacheMatrix <- function(x = matrix()){
  m <- NULL
  
# reset the parent ( makeCacheMatrix) environment variables
  set <- function(y){
    x <<- y
    m <<- NULL
  }
  
  # get the current value of x
  get <- function() x
  
  # set the inverse to m variable of parent ( makeCacheMatrix) environment
  setInverse <- function(solve) m <<- solve
  
  # get the current value of m
  getInverse <- function() m
  
  # create a named list so that these objects can be used on downstream environment using $ operator.
  list(set=set,get=get,setInverse=setInverse,getInverse=getInverse)
}

#Calculate Inverse of matrix with caching enabled
catchSolve <- function(x,...){
  m <- x$getInverse()
  if(!is.null(m)) {
    message("getting cached data")
    return(m)
  }
  mx_data <- x$get()
  m <- solve(mx_data,...)
  x$setInverse(m)
  return(m)
}
