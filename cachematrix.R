## Functions to:
## *create a special "matrix" object that can cache its inverse
## *compute the inverse of the special "matrix", cache and return it

## Function 'makeCacheMatrix' recieves a normal matrix, changes it to a
## |special 'matrix' whose inverse calculation can be cached
## Usage format: special_matrix <- makeCacheMatrix(normal_matrix)

makeCacheMatrix <- function(x = matrix()) {
  ## 'm' will be used to store the inverse
  m <- NULL
  
  ## 'x$set' function only assigns the value of the original matrix
  set <- function(y) {
    x <<- y
    m <<- NULL
  }
  
  ## 'x$get' function only retrieves the value of the original matrix
  get <- function() {
    x }
  
  ## 'x$setinverse' is the assignment function for the inverse of the 
  ## | original matrix
  setinverse <- function(solve) {
    m <<- solve }
  
  ## 'x$getinverse' function only retrieves the inverse of the original matrix
  getinverse <- function() {
    m }
  
  ## The returned list contains the parameters of our special 'matrix'
  list(set = set, get = get, setinverse = setinverse, getinverse = getinverse) 
}

## Function 'cacheSolve' calculates, caches and returns the (cached) inverse
## |of the returned 'matrix' of  makeCacheMatrix, as well as indicating if that
## |value has been previously calculated
## Usage format: cacheSolve(special_matrix)

cacheSolve <- function(x, ...) {
  ## Return a matrix that is the inverse of matrix 'x'
  
  ## Retrieve inverse and assign to m
  m <- x$getinverse()
  
  ## Check if m has a value (i.e. if the inverse has been calculated before),
  ## |indicate if so
  if(!is.null(m)) {
    message("getting cached data")
    return(m)
  }
  
  ## Else, calculate inverse
  data <- x$get()
  m <- solve(data, ...)
  
  ## Store inverse in special_matrix 'setinverse' column
  x$setinverse(m)
  
  ## Return m
  m
}
