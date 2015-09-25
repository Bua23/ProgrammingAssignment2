##makeCacheMatrix is a function that stores a martix and a cached value of the inverse of the  matrix.
##It returns a list of functions which do the following:
##              1. set the matrix
##              2. get the matrix
##              3. set the inverse
##              4. get the inverse
##         This is used as the input to cacheSolve()


## creates a special "matrix" object that can cache its inverse

makeCacheMatrix <- function(x = matrix()) {
  m<-NULL
  set<-function(y){
    x<<-y
    m<<-NULL
  }
  get<-function() x
  setmatrix<-function(solve) m<<- solve
  getmatrix<-function() m
  list(set=set, get=get,
       setmatrix=setmatrix,
       getmatrix=getmatrix)
}


##  This function computes the inverse of the "matrix" returned by makeCacheMatrix(). 
## If the inverse has already been calculated and the matrix has not been updated, 
## This function will retrieve the inverse from the cache directly.

cacheSolve <- function(x, ...) {
          m<-x$getmatrix()
  if(!is.null(m)){
    message("getting cached data")
    return(m)
  }
  matrix<-x$get()
  m<-solve(matrix, ...)
  x$setmatrix(m)
  m
}

