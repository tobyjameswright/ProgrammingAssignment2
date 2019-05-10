## Put comments here that give an overall description of what your
## functions do

## Write a short comment describing this function
## makeCacheMatrix set the value of a matrix, then gets the value of the matrix
## creates the function to get and set the inverse
makeCacheMatrix <- function(x = matrix()) {
  m <- NULL
  set <- function(y) {
    x <<- y
    m <<- NULL
  }
  get <- function() x
  setinv <- function(inv) m <<- inv
  getinv <- function() m
  list(set = set, get = get,
       setinv = setinv,
       getinv = getinv)
}


## Write a short comment describing this function
## cacheSolve checks whether the inverse has already been calculated and if not 
## calculates inverse and stores to cache

cacheSolve <- function(x, ...) {
  m <- x$getinv()
  if(!is.null(m)) {
    message("getting cached data")
    return(m)
  }
  data <- x$get()
  m <- solve(data, ...)
  x$setinv(m)
  m
        
}

#test funcs
#my_matrix <- makeCacheMatrix(matrix(1:4, 2, 2))
#my_matrix$get()
#my_matrix$getinv()
#cacheSolve(my_matrix)
