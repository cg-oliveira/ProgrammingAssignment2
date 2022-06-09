## Put comments here that give an overall description of what your
## functions do

## Write a short comment describing this function

makeCacheMatrix <- function(x = matrix()) {
  inv <- NULL            #initialize the variable
  set <- function(y) {   #function that sets the matrix
    x <<- y
    inv <<- NULL
  }
  get <- function() x    #function that gets the matrix
  setinv <- function(inverse) inv <<- inverse   #function that sets the matrix inverse
  getinv <- function() inv                      #function that gets the inverse matrix
  list(set = set, get = get,        #list with the result'
       setinv = setinv,
       getinv  = getinv)
}


## Write a short comment describing this function

cachesolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
  inv <- x$getinv()               #gets the inverse of x
  if(!is.null(inv)) {             #if the inverse is already cached, returns inv
    message("getting cached data")
    return(inv)
  }
  data <- x$get()                 #otherwise gets the matrix
  inv <- solve(data, ...) 
  x$setinv(inv)                   #calculates the inverse
  inv                             #returns the inverse
}
