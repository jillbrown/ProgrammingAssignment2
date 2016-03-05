## Assignment to cache the inverse of a matrix.

## Set value of empty matrix, get the value of new matrix,
## set inverse, return inverse

makeCacheMatrix <- function(x = matrix()) {
  m <- NULL
  set <- function(y) {
    x <<- y
    m <<- NULL
  }
  get <- function() x
  setmatrix <- function(solve) m <<- solve
  getmatrix <- function() m
  list(set = set, get = get,
       setmatrix = setmatrix,
       getmatrix = getmatrix)
}


## calculates inverse if the inverse isn't present.

cacheSolve <- function(x=matrix(), ...) {
  ## Return a matrix that is the inverse of 'x'
  m <- x$getmatrix()
  if(!is.null(m)) {
    message("getting cached data")
    return(m)
  }
  data <- x$get()
  m <- solve(data, ...)
  x$setmatrix(m)
  m
}


## testing

test <- makeCacheMatrix()
test$set(matrix(1:9,3,3))
cacheSolve(test)
