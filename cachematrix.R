## These two functions together serve to store the inverse matrix in cache.
## The first function creates a "matrix" - a list of functions that serve to operate with matrix and with its invertion.
## The second function searches for inverse for a "matrix" specified by argument.
## If the inverse hasn't been calculated yet, it will be calculated and stored to cache.

## Function makeCacheMatrix takes a "matrix" as an argument. After that, the inv variable is set to NULL value,
## because the inversion hasn't been calculated yet.
## The function returns a list of 4 functions to operate with a matrix and its inverse

makeCacheMatrix <- function(x = matrix()) {
  
  inv <- NULL                             
  set <- function(y) {                  
    x <<- y                             
    inv <<- NULL                          
  }
  get <- function() x                   
  setinverse <- function(inverse) inv <<- inverse  
  getinverse <- function() inv               
  list(set = set, get = get,             
       setinverse = setinverse,
       getinverse = getinverse)
  
}


## Function cacheSolve takes a "matrix" (created by makeCacheMatrix function) and tries to set the value from cached data
## if this operation is succesfull (an inverse has already been calculated), than the message "getting cached data" is displayed
## and function returns an inverse matrix from cahce. 
## In the opposite case, function checks if the input matrix is square. If it's not the case, message about mistake is displayed
#and NULL value is returned (and also saved in cache)
## if the matrix is square, the inverse matrix is calculated using solve() function, the result is saved in cache and also
## returned by the funcion. 

cacheSolve <- function(x, ...) {
  ## Return a matrix that is the inverse of 'x'
  
  inv <- x$getinverse()                     
  if(!is.null(inv)) {                     
    message("getting cached data")     
    return(inv)                          
  }
  
  data <- x$get()                       
  
  if (nrow(data) != ncol(data)){
    message("impossible to calculate invertion of rectangular matrix")
    inv <- NULL
    return(inv)
  }
  else  inv <- solve(data, ...)                  
  x$setinverse(inv)                          
  inv                       
}
