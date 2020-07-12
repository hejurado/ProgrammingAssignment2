#makeCacheMatrix
#The first function, makeCacheMatrix creates
#an special "matrix",
#set the value of the matrix
#get the value of the matrix
#set the value of the inverse
#get the value of the inverse
makeCacheMatrix <- function(x = matrix()) {
  i <- NULL
  set <- function(y) {
    x <<- y
    i <<- NULL
  }
  get <- function() x
  setInverse <- function(inverse) i <<- inverse
  getInverse <- function() i
  list(set = set,
       get = get,
       setInverse = setInverse,
       getInverse = getInverse)
}


#cacheSolve
#The following function calculates the inverse of the special "matrix"
#created with the above function.
#However, it first checks to see if the inverse matrix has already been calculated.
#If so, it gets the inverse from the cache and skips the computation.
#Otherwise, it calculates the inverse of the data and sets the value of the inverse
#in the cache via the setInverse function.
cacheSolve <- function(x, ...) {
  ## Return a matrix that is the inverse of 'x'
  i <- x$getInverse()
  if(!is.null(i)) {
    message("getting cached data")
    return(i)
  }
  data <- x$get()
  i <- solve(data, ...)
  x$setInverse(i)
  i

}

## test matrix
C<-matrix(ncol=3,c(2,4,3,5,7,1,2,2,3),byrow=TRUE)

# I create special matrix
MI=makeCacheMatrix(C)
# use cacheSolve to calculate inverse matrix
cacheSolve(MI)

C<-matrix(ncol=3,c(2,4,3,5,7,1,2,2,1),byrow=TRUE)
MI=makeCacheMatrix(C)
cacheSolve(MI)
