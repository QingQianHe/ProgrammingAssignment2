## Put comments here that give an overall description of what your
## functions do
## The following two functions are used to cache the inverse of a matrix, 
## so that it does not need to compute repeatedly when the contents of 
## a matrix do not change.

## Write a short comment describing this function
## The following function creates a special"matrix"object that can cache its inverse,
## It returns a list containing a function to 1)get the value of a matrix,
## 2) set the inversed matrix, and 3) get the inversed matrix.
makeCacheMatrix <- functicon(x = matrix()) {
  InvMatrix<-NULL
  get<-function()x
  setInvMatrix<-function(IMatrix) InvMatrix <<-IMatrix
  getInvMatrix<-function()InvMatrix
  list(get=get,setInvMatrix=setInvMatrix,getInvMatrix=getInvMatrix)
}

## Write a short comment describing this function
## This function computes the inverse of the special"matrix"returned by
## the makeCacheMatrix above.It first checks to see if the inverse has
## already been computed. if so, it gets the inversed matrix and returns
## it. Otherwise, it computes the inverse of a matrix and set it in the
## caches via the setInvMatrix function.
cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
  InvMatrix<-x$getInvMatrix()
  if(!is.null(InvMatrix)){
    return(InvMatrix)
  }
  data<-x$get()
  InvMatrix<-solve(data,...)
  x$setInvMatrix(InvMatrix)
  InvMatrix
}
