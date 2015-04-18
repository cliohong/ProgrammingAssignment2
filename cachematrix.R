
makeCacheMatrix<-function(x=matrix()){
  inverse<-NULL   ##initially assinging null value to inverse
  
  set<-function(y){
    x<<-y       ##setting matrix 'x'
    inverse<<-NULL
  }
  get<-function()x ##returning  matrix 'x'
  set_inverse<-function(solve) inverse<<-solve ##preserve the value of the inverse
  get_inverse<-function()inverse ##returning the inverse 
  
  list(set=set, get=get, set_inverse=set_inverse, get_inverse=get_inverse)
}
## since the function cacheSolve is created with the makeCacheMatrix function, it  returns the inverse of the matrix, 
##  and if the cached inverse does work, cacheSolve function retrieves it, otherwise it computes, preserves and returns 
##it.
 cachSolve<-function(x,...){   ## return the matrix which is the inverse of 'x'
   inverse<-x$get_inverse()   ## getting inverse
   if(!is.null(inverse)){      ##checking the presense of inverse
     message("getting cached data")  ##displaying the message
     return(inverse)    ##inverse returns
   }
   data<-x$get()    ##getting matrix 
   inverse<-solve(data, ...)  ## using solve() to compute inverse
   x$set_inverse(inverse) ## caching the inverse
   inverse  ##  returning inverse
   
 }
