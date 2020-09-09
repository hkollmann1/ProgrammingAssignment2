 ##The MakeCacheMatrix function is used to create the special matrix 
##to obtain the inverse

makeCacheMatrix <- function(x = matrix()) {
m <- NULL
  set <- function(y) {
    x <<-y
    m<<-NULL
    
  }
  get <- function() x
  setsolve <- function(solve) m <<-solve
  getsolve <- function() m
  list(set = set, get=get, setsolve=setsolve, getsolve=getsolve)
  
}


## The cachesolve function calculates the inverse 
##of the matrix obtained by the MakeCacheMatrix function

cacheSolve <- function(x, ...) {
	m <- x$getsolve()
  if(!is.null(m)){
    message("obteniendo datos del cache")
    return(m)
  }
  data <- x$get()
  m <- solve(data, ...)
  x$setsolve(m)
        ## Return a matrix that is the inverse of 'x'
}

## my test code
##m1 <- matrix(c(1/2, -1/4, -1, 3/4), nrow = 2, ncol = 2)
##> mymatrix <- makeCacheMatrix(m1)
##  > mymatrix$get()
##[,1]  [,2]
##[1,]  0.50 -1.00
##[2,] -0.25  0.75
##> cacheSolve(mymatrix)
##obteniendo datos del cache
##[,1] [,2]
##[1,]    6    8
##[2,]    2    4
##> n2 <- matrix(c(5/8, -1/8, -7/8, 3/8), nrow = 2, ncol = 2)
##> mymatrix$set(n2)
##obteniendo datos del cache
##[,1] [,2]
##[1,]    3    7
##[2,]    1    5
