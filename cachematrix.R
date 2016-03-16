## Put comments here that give an overall description of what your
## functions do

## Write a short comment describing this function

makeCacheMatrix <- function(x = matrix()) {
  inv<-NULL                                  #set up output data set for inverse matrix,start with NULL
  set<-function(y){                          #set function allow R to change input matrix
    x<<-y               
    inv<<-NULL
  }
  get<-function()x                           #get function allow R to obtain orginal matrix
  setinv<-function(inverse) inv<<-inverse    #setinv function allow R to change inverse matrix
  getinv<-function()inv                      #getinv funciton allow R to get inverse matrix
  list(set=set,get=get,setinv=setinv,getinv=getinv) #set up lists for cacheSolve to quote
  
}



## Write a short comment describing this function

cacheSolve <- function(x, ...) {
  inv<-x$getinv()
  if(!is.null(inv)){
    message("getting cached data")
    return(inv)
  }
  data<-x$get()
  inv <- solve(data,...)
  x$setinv(inv)
  inv
        ## Return a matrix that is the inverse of 'x'
}
