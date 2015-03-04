## Overall
## Use makeCacheMatrix function with input as a invertible matrix.
## Like: cm<-makeCacheMatrix(matrix(0:3,2,2))
## Now cacheSolve(cm) - this function will return the inverse of the matrix 
## defined above. It will caclculate the inverse if that matrix had not been inverted before,
## else it will not calculate, rather will get the result from previous run.

## makeCacheMatrix
## This function uses the feature of lexical scoping and use of '<<-'
## to assign a value which can be used outside current function's environment.
## It helps it to retain the inverse value (to be calculated in the following funtion)
## and use that. Whenever the input matrix is changed with cm$set, inverse value is again set
## to null and will be recalculated in the following function.

makeCacheMatrix <- function(x = matrix(c(1:4),2,2)) {
  i <- NULL
  set <- function(y) {
    x <<- y
    i <<- NULL
  }
  get <- function() x
  setinv <- function(inv) i <<- inv
  getinv <- function() i
  list(set = set, get = get,
       setinv = setinv,
       getinv = getinv)
}


## cacheSolve
## This function first recalls whether inverse of same matrix is requested from 
## the previous run. If it is, then just prints a message and the result.
## Else, it calculates the inverse and again stores that value for future use.

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
  i <- x$getinv()
  if(!is.null(i)) {
    message("getting cached data")
    return(i)
  }
  data <- x$get()
  i <- solve(data, ...)
  x$setinv(i)
  i
}

## Example
## cm<-makeCacheMatrix(matrix(0:3,2,2))
## cacheSolve(cm)
## cacheSolve(cm)
## cm$set(matrix(1:4,2,2))
## cacheSolve(cm)
## cacheSolve(cm)