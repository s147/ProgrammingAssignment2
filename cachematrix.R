## The following function will help us cache when we need to find the inverse
##of a matrix which is time consuming. We will be able to access the inverse in other
## environments as well without computing them again.

## The makeCacheMatrix contains a list of functions which will cache its inverse.

makeCacheMatrix <- function(x = matrix()) {
  m <- NULL
  set <- function(y){
    x <<- y
    m <<- NULL
  }
  
  get <- function() x
  setinmat <- function(solve) m <<- solve
  getinmat <- function() m
  list(set = set, get = get, setinmat = setinmat, 
       getinmat = getinmat)
}


## The casheSolve function will get the inverse of the matrix without any time-Consuming computations
## if it is vailable, otherwise it will compute its matrix and set it for further use.

cacheSolve <- function(x, ...) {
  ## Return a matrix that is the inverse of 'x'
  m <- x$getinmat
  if(!is.null(m)){
    message("getting cached data")
  }
  data <- x$get()
  m <- solve(data,...)
  x$setinmat(m)
  m
}
