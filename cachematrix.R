## Creates a special matrix object that is able to cache its inverse
## Then computes the inverse of the matrix

## set, get, set inverse and get inverse of matrix

makeCacheMatrix <- function(x = matrix()) {
  i <- NULL
  set <- function(y) {
    x <<- y
    i <<- NULL
  }
  get <- function() x
  setinv <- function(inv) i <<- inv
  getinv <- function() i
  list(set = set, get = get, setinv = setinv, getinv = getinv)
}


## Return the matrix inverse

cacheSolve <- function(x, ...) {
  ## Return a matrix that is the inverse of 'x'
  i <- x$getinv()
  if(!is.null(i)) {
    message("getting cached data")
    return(i)
    
  }
  m <- x$get()
  i <- solve(m)
  x$setinv(i)
  i
}
