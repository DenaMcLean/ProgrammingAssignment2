## This program is designed to improve execution efficeny appling inverse calculation to a matrix
## This code will 1) create a matrix and populate the matrix with four functions required to 
## perform inverse operation on a square matrix
## 2) the inverse function will be stored in cache for more efficient processing


##set the value of the vector
##get the value of the vector
##setinv the value of the inverse
##getinv the value of the inverse
## the following 4 commands can be used to test the code
## rnum=rnorm(100)
## matTest=matrix(rnum,nrow=10,ncol=10)
## special<-makeVector(matTest)
##cacheinv(special)
##cacheinv(special) a second time will utilize the cached data
makeVector <- function(x = numeric()) {
  m <- NULL
  set <- function(y) {
    x <<- y
    m <<- NULL
  }
  get <- function() x
  setinv <- function(inverse) m <<- inverse
  getinv <- function() m
  list(set = set, get = get,
       setinv = setinv,
       getinv = getinv)
}

## cache function will apply inverse operation to a matrix

cacheinv <- function(x, ...) {
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
