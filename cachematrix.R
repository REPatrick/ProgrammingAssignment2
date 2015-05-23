## Matrix Inverse caching object

## create a matrix "wrapper" that adds support for a cached inverse matrix
makeCacheMatrix <- function(x = matrix()) {
  i <- NULL
  set <- function(y) {
    x <<- y
    i <<- NULL
  }
  get <- function() x
  setinverse <- function(inv) i <<- inv
  getinverse <- function() i;
  return(list(set = set, get = get,
              setinverse = setinverse,
              getinverse = getinverse))
}

## Returns a matrix that is the inverse of 'x', caches the result
## so the calculation is done only once, even if called multiple times
cacheSolve <- function(x, ...) {
  inv <- x$getinverse()
  if(!is.null(inv)) {
    message("getting cached data")
    return(inv)
  }
  data <- x$get()
  message("calculating inverse")
  inv <- solve(data, ...)
  x$setinverse(inv)
  return(inv)
}


## my testing code
testMatrix <- matrix(trunc(rnorm(4*4)*100),4,4) 

wrappedTestMatrix <- makeCacheMatrix(testMatrix)
print(wrappedTestMatrix)

invertedTextMatrix1 <- cacheSolve(wrappedTestMatrix);
print(invertedTextMatrix1)

invertedTextMatrix2 <- cacheSolve(wrappedTestMatrix);
print(invertedTextMatrix1)