## Put comments here that give an overall description of what your
## functions do

## Write a short comment describing this function

makeCacheMatrix <- function(x = matrix()) {             #set up a function that require x as matrix
 inv<-NULL                                              #prepare the outcome matrix as inv and set it to null
  set<-function(y){                                     #This function is use to change value of x to y and set inv to NULL
    x<<-y                                               #while privided a new matrix
    inv<<-NULL
  }
  get<-function()x                                      #to get the matrix input
  setinv<-function(inverse) inv<<-inverse               #use to change inv value if we have new result
  getinv<-function()inv                                 #get original inv value
  list(set=set,get=get,setinv=setinv,getinv=getinv)     #provide list for cacheSolve function to quote
  
}


## Write a short comment describing this function

cacheSolve <- function(x, ...) {

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
