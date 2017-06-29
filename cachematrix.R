## These functions that cache the inverse matrix.
## I want to create object that stores matrix and cache its inverse.

## this function creates a special "matrix" object that can cache its inverse

makeCacheMatrix <- function(x = matrix()) {
 inv<-NULL
 set<-function(y){
   x<<-y
   inv<<-NULL
 }
 get<-function(){
   x
   }
 setInverse<-function(inverse)inv<<-inverse
 getInverse<-function()inv
 list(set=set,get=get,setInverse=setInverse,getInverse=getInverse)

}


## This function computes the inverse of the special "matrix" returned by makeCacheMatrix above. 
## If the inverse has already been calculated (and the matrix has not changed).
## cacheSolve should retrieve the inverse from the cache.

cacheSolve <- function(x, ...) {
  inv<-x$getInverse()
  if(!is.null(inv)){
    message("getting cached data")
    return(inv)
  }
  inve<-x$get()
  inv<-solve(inve)
  x$setInverse(inv)
  inv
        ## Return a matrix that is the inverse of 'x'
}
