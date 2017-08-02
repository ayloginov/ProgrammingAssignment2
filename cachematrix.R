## The functions below calculate the inverse of a matrix and store 
## the results in cache. Cached result can be retrieved instead of 
## repeated recalculations of the inverse.

## makeCacheMatrix function creates a special object that stores 
## the matrix (provided as argument) and its inverse.
## makeCacheMatrix function comprises four functions (set, get, setinv, getinv)
## and two data objects x and inverse.

## start by initializing data objects. Matrix x is initialized as function
## argument, inverse is set to NULL (empty object to be used later)
makeCacheMatrix <- function(x = matrix()) {
  inverse <- NULL
  ## define the set() function with an input argument y. Set() assignes 
  ## the new input argument y to the object x in the parent environment, and
  ## cleares any value of inverse that had been cached from prior calculation.
  set <- function(y) {
    x <<- y
    inverse <<- NULL
  }
  ## define get() function. This returns x stored in the main function.
  get <- function() x
  ## define setinv() and getinv() functions. setinv() stores the value of the
  ## inverse in the main function, getinv() returns it.
  setinv <- function(inv) inverse <<- inv
  getinv <- function() inverse
  ## store the four functions defined above within the parent function 
  list(set = set, get = get, setinv = setinv, getinv = getinv)
}


## cacheSolve retrieves or populates the inverse of an object from makeCacheMatrix.
## The function first attempts to retrieve the inverse stored in cache. If cache 
## has been reset to NULL the function recalculates the inverse and stores it in
## in the object of makeCacheMatrix
cacheSolve <- function(x, ...) {
  ## call getinv() on the input object and verify that stored value exists
  ## (i.e. not equal to NULL). If the value exists the function returns it
  ## with a message that the data is retrieved from cache.
  inv <- x$getinv()
  if(!is.null(inv)) {
    message("getting cashed data")
    return(inv)
  }
  ## If cache has been reset to NULL get() calls for the data from the input
  ## object, solve() calculates the new inverse which is then stored in the 
  ## input object by setinv()
  data <- x$get()
  inv <- solve(data, ...)
  x$setinv(inv)
  inv
  }

## example usage 
## see details at https://www.coursera.org/learn/r-programming/discussions/weeks/3/threads/ePlO1eMdEeahzg7_4P4Vvg
## create matrix m1
m1 <- matrix(c(1/2, -1/4, -1, 3/4), nrow = 2, ncol = 2)
## supply m1 as argument to makeCacheMatrix and save the result as my_matrix_object
my_matrix_object <- makeCacheMatrix(m1)
## call cacheSolve function on my_matrix_object
cacheSolve(my_matrix_object)
## the result is the inverse matrix for m1
## if we repeat the call for cacheSolve function on my_matrix_object the function
## will return the cached value with the respective message
## create another matrix n1
n1 <- matrix(c(5/8, -1/8, -7/8, 3/8), nrow = 2, ncol = 2)
## update my_matrix_object with the new data for argument x
my_matrix_object$set(n1)
## cacheSolve recalculates and stores the new inverse matrix
cacheSolve(my_matrix_object)


