## The following program adds two functions. One for creating matrix objects that 
## can cache its inverse with corresponding methods to update them.

## makeCacheMatrix creates a special "matrix", which is really a list containing
## functions to:
## 1. set the matrix
## 2. get the matrix
## 3. set the inverse of the matrix
## 4. get the inverse of the matrix

makeCacheMatrix <- function(x = matrix()) {
  mat.inv <- NULL
  set <- function(y) {
    x <<- y
    mat.inv <<- NULL
  }
  get <- function() {x}
  setInverse <- function(inverse) {mat.inv <<- inverse}
  getInverse <- function() {mat.inv}
  return(list(set = set, get = get, setInverse = setInverse, getInverse = getInverse))
}


## The following function calculates the inverse of the special "matrix" created
## with the above function. However, it first checks to see if the inverse has
## already been calculated. If so, it gets the inverse from the cache and skips the
## computation. Otherwise, it calculates the inverse of the matrix and sets the value
## of the inverse in the cache via the setInverse function.

cacheSolve <- function(x, ...) {
  mat.inv <- x$getInverse()
  if(!is.null(mat.inv)) {
    message("getting cached data")
    return(mat.inv)
  }
  mat <- x$get()
  mat.inv <- solve(mat, ...)
  x$setInverse(mat.inv)
  return(mat.inv)
}

## An example follows:

## We create a 'matrix' object. At first, it holds no cache of its inverse.
A <- makeCacheMatrix(diag(3))

## Should return null
A$getInverse()

## Should calculate the matrix inverse and store it in the object's cache
cacheSolve(A)

## We can now call the inverse through the getInverse method
A$getInverse()


## Let's change the matrix with it's set function
A$set(diag(5))

## Again, no cache should be stored
A$getInverse()

## The cacheSolve caclulates and stores the new inverse
cacheSolve(A)

## We can now call the inverse through the getInverse method
A$getInverse()
