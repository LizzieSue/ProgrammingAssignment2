## Programming Assignment 2 - Caching the inverse of a matrix ##
## first function creates special matrix to cache inverse
## second function computes inverse of special matrix from first function

## This function creates a special "matrix" object that can cache its inverse.

makeCacheMatrix <- function(x = matrix()) { #x initialized with default empty matrix
  m<-NULL #initializing m in makeCacheMatrix environment
  set<-function(y){ #
    x<<-y #assigns input y to matrix x in parent environment
    m<<-NULL #reassigns m as NULL to clear any previously cached value
  }
  get<-function() x #retreives matrix x from parent environment
  setinv<-function(solve) m<<-solve #assigns new inverse value to m in parent environment
  getinv<-function() m #retreives inverse m from parent environment
  list(set=set, get=get, setinv=setinv, getinv=getinv) #each function returned as named 
    #object in list to be returned to parent environment
}


## This function computes the inverse of the special "matrix" returned by 
  #`makeCacheMatrix` above. If the inverse has already been calculated (and the matrix 
  #has not changed), then `cacheSolve` should retrieve the inverse from the cache.

cacheSolve <- function(x, ...) {
  m<-x$getinv() #calls getinv function on input object x and assigns to m
  if(!is.null(m)){ #checks if inverse matrix is NULL (new matrix as input sets m to NULL)
    message("getting cached data")
    return(m) #returns the valid, cached inverse matrix if not NULL
    }
  data<-x$get() #if m is NULL (!is.null(m)=FALSE), get matrix from input
  m<-solve(data,...) #calculate inverse of new matrix
  x$setinv(m) #sets inverse of input matrix
  m #prints inverse matrix
}

#test#
#m1 <- matrix(c(1/2, -1/4, -1, 3/4), nrow = 2, ncol = 2)
#n1 <- matrix(c(6,2,8,4), nrow = 2, ncol = 2)
#myMatrix_object <- makeCacheMatrix(m1)
#cacheSolve(myMatrix_object)

#n2 <- matrix(c(5/8, -1/8, -7/8, 3/8), nrow = 2, ncol = 2)
#myMatrix_object$set(n2)
#cacheSolve(myMatrix_object)
