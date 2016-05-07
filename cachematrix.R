
## R PROGRAMMING - WEEK 3
## ASSIGNMENT : CACHING THE INVERSE OF A MATRIX
## ALEXIS VAN ESPEN


makeCacheMatrix <- function(x = matrix()) {
  # makeCacheMatrix returns a list of functions.
  # It caches the value of an input matrix and of its inverse.
  # Please note that makeCacheMatrix does not compute the inverse.
  # This is the job of the function 'cacheSolve'.
  
  # Every time we call the function, the inverse is reinitialized.
  # This is what we want : every time we pass a new matrix, we empty the cache.
  matrix.inverse <- NULL
  
  # The function 'set' can be used to initialize makeCacheMatrix with a new matrix.
  # Lexical scoping is key here: y is copied to x, which is the argument of makeCacheMatrix.
  # The same applies to the matrix.inverse, which is set to NULL again.
  # Hence the function 'set' reinitializes the input matrix (to the value of y) and its inverse (NULL).
  # If we hadn't used the assignement operator <<-, we would have created new local variables.
  # This is obvioulsy not what we want here.
  set <- function(y) {
    x <<- y
    matrix.inverse <<- NULL
  }
  
  # The function 'get' retrieves the value of the input matrix.
  # Lexical scoping applies here, since x is a free variable in the function 'get'.
  # The value of x is retrieved in the environment where 'get' was defined.
  # This environement is the makeCacheMatrix function.
  get <- function() x
  
  # The function 'set.inverse' is used to assign a value to the matrix.inverse variable.
  # 'set.inverse' doesn't make the computation. It uses as an argument the inverse computed in CacheSolve.
  # Thanks to the <<- operator, the function does not create a new local variable called matrix.inverse.
  # It uses the variable named 'matrix.inverse' in the environment where the function was defined.
  # This environment is the function makeCacheMatrix. 
  set.inverse <- function(inverted.matrix) matrix.inverse <<- inverted.matrix
  
  # The function 'get.inverse' retrieves the value of the inverse of the matrix.
  # Lexical scoping applies here, since matrix.inverse is a free variable in the function 'get.inverse'.
  # The value of matrix.inverse is retrieved in the environment where 'get.inverse' was defined.
  # This environment is the makeCacheMatrix function.
  get.inverse <- function() matrix.inverse
  
  # The function returns the result of the last computation.
  # Here, it's a list of functions.
  list(set = set, 
       get = get,
       set.inverse = set.inverse,
       get.inverse = get.inverse)

} # end makeCacheMatrix


cacheSolve <- function(x, ...) {
  # Return a matrix that is the inverse of 'x'.
  # Please note that the argument x should be created with makeCacheMatrix.
  # Passing a matrix to this function will yield an error.
  
  # The variable matrix.inverse defined here is not the same as the variable matrix.inverse defined inside makeCacheMatrix.
  # Here, the local variable matrix.inverse uses the function get.inverse (which is an element of x).
  # get.inverse retrieves the most recent value of the inverse.
  matrix.inverse <- x$get.inverse()
  
  # If the inverse is not NULL, it has been cached before.
  # In this case, return the cached value with a message.
  # The return statement will prevent further execution of the function.
  if (!is.null(matrix.inverse)) {
    message("getting cached data")
    return(matrix.inverse)
  }
  
  # If the inverse is NULL, it must be because the input matrix has changed.
  #   (This could happen through a call to makeCacheMatrix of through the function 'set'.
  #   The difference is that makeCacheMatrix will create a whole new list, 
  #   while set applies on an object already created with makeCacheMatrix, and will only update the input matrix.)
  #   
  # So, if the input matrix has changed, retrieve its updated value...
  input.matrix <- x$get()
  
  # ... and compute its inverse.
  # This is where we use the function 'solve'.
  matrix.inverse <- solve(input.matrix, ...)
  
  # The inverse is then passed to 'set.inverse' 
  # (which will copy it to the matrix.inverse variable of 'makeCacheMatrix').
  x$set.inverse(matrix.inverse)
  
  # The function cacheSolve returns the inverse of the input matrix.
  matrix.inverse
  
} # end cacheSolve
