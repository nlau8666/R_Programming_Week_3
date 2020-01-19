## Put comments here that give an overall description of what your
##operator assigning a value to an object in an environment that is different from the current environment.

## makeCacheMatrix is a function that contains a list to
##set the value of the matrix
##get the value of the matrix
##set the value of the matrix's inverse
##get the value of the matrix's inverse

##cacheSolve computes the inverse of the matrix returned by makeCacheMatrix
##if the inverse is already calculated it will retrieve it from its cache (assuming the matric hasnt changed)

makeCacheMatrix <- function(x = matrix()) { ##default value of x is an empty matrix. This is important b/c if its not, x$get wont work
  inv <- NULL ##inv is set to NULL b/c it will be used in later code
  set <- function(y) {
    x <<- y 
    inv <<- NULL
  }
  get <- function() x ##gets the value of the matrix
  setInverse <- function(inverse) inv <<- inverse ##sets value of the inverse matrix
  getInverse <- function() inv  ##gets the value of the inverse matrix
  list(set = set,
       get = get,
       setInverse = setInverse,
       getInverse = getInverse)
}

cacheSolve <- function(x, ...) { ##calculates/retrieves the matrix inverse
  ## Return a matrix that is the inverse of 'x'
  inv <- x$getInverse() 
  if (!is.null(inv)) { ##checks if the inverse is already calculated, if yes displays message below
    message("getting cached data")
    return(inv)
  }
  mat <- x$get() ##if inverse not previously calculated, it will do so now
  inv <- solve(mat, ...)
  x$setInverse(inv)
  inv
}
##test 1
x<-makeCacheMatrix(matrix(1:4,nrow=2,ncol=2))
x$get()
x$getInverse()
cacheSolve(x)
##test 2
x<-makeCacheMatrix(matrix(c(2,2,1,2),2,2))
x$get()
x$getInverse()
cacheSolve(x)
##test 3
x<-makeCacheMatrix(matrix(c(2,2,3,2),2,2))
x$get()
x$getInverse()
cacheSolve(x)
cacheSolve(x)