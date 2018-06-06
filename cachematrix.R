## Put comments here that give an overall description of what your
## functions do
# MakeCacheMatrix once binded to an object will produce a list of 4 items which will store all functions.
# CahceSolve will take in the object and call functions available in the environment scope and inverse the 2x2 matrix
#it will then store the output of the computation in 'm' wherease before it was 'null' until the function
# is run.



## Write a short comment describing this function
#pass in a matrix object set local variables and declare 4 functions which can be binded to an object.

makeCacheMatrix <- function(x = matrix()) {
  m <- NULL
  set <- function(y){
    x <<- y
    m <<- NULL
  }
  get <- function() x
  setinverse <- function(inverse) m <<- inverse
  getinverse <- function() m
  list(set = set, get = get,
       setinverse = setinverse,
       getinverse = getinverse)
}



## Write a short comment describing this function
# pass in object which is a list of 4.
# run get inverse and store results in m
# if 'm' is not null then return 'm' and cache message otherwise
#set data to the matrix object
# then run set inverse function over 'm' resulting in inversing the matrix
# set the result to 'm'in the binded makeCacheMatrix environment
# print m to r console
cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
  m <- x$getinverse()
  if(!is.null(m)) {
    message("getting cached data")
    return(m)
  }
  data <- x$get()
  m <- solve(data, ...)
  x$setinverse(m)
  m
}



# Testing 
test <- matrix(c(1,2,3,4),2,2)
test_bind <- makeCacheMatrix(A)
cacheSolve(test_bind) # setinverse value to result
cacheSolve(test_bind) #navigates the if statement to return calculated computation from getinverse()