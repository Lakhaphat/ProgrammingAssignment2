## This programming exercise involves the creation of functions which
## can be used to cache the inverse of an invertible matrix.

## The makeMatrix function contains a list that performs the following operations:
## (1) Set the value of the matrix
## (2) Get the value of the matrix
## (3) Set the value of the Inverse Matrix
## (4) Get the value of the Inverse Matrix

makeMatrix <- function(x = matrix()) {
  i <- NULL
  set <- function(y) {
    x <<- y
    i <<- NULL
  }
  get <- function() x
  setinverse <- function(inverse) i <<- inverse
  getinverse <- function() i
  list(
    set = set,
    get = get,
    setinverse = setinverse,
    getinverse = getinverse
  )
}

## This function reuses the cached result whenever it is available and is
## used in conjunction with the special "matrix" defined above.

cacheSolve <- function(x, ...) {
  i <- x$getinverse()
  if(!is.null(i)) {
    message("getting cached data")
    return(i)
  }
  m <- x$get()
  i <- solve(m, ...)
  x$setinverse(i)
  i
}

## How to use and computed example: 
## (1)source('cachematrix.R')
## (2)Create the matrix you want to compute the inverse this way:
## test_matrix <-makeMatrix(matrix(c(1,2,2,1),c(2,2)))
## cacheSolve(test_matrix)
##    [,1]       [,2]
##[1,] -0.3333333  0.6666667
##[2,]  0.6666667 -0.3333333
