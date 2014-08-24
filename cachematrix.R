## Functions to:
## *create a special "matrix" object that can cache its inverse
## *compute the inverse of the special "matrix", cache and return it

## Function 'makeCacheMatrix' recieves a normal matrix, changes it to a
## |special 'matrix' whose inverse calculation can be cached
## Usage format: special_matrix <- makeCacheMatrix(normal_matrix)

makeCacheMatrix <- function(x = matrix()) {
  m <- NULL
  set <- function(y) {
    x <<- y
    m <<- NULL
  }
  get <- function() x
  setinverse <- function(solve) m <<- solve
  getinverse <- function() m
  list(set = set, get = get,
       setinverse = setinverse,
       getinverse = getinverse) 
}

## Function 'cacheSolve' calculates, caches and returns the (cached) inverse
## |of the returned 'matrix' of  makeCacheMatrix, as well as indicating if that
## |value has been previously calculated
## Usage format: cacheSolve(special_matrix)

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
