## Put comments here that give an overall description of what your
## functions do

## Write a short comment describing this function
## The function, makeCacheMatrix creates a special "matrix", which is really a list containing a function to
##set the value of the matrix
##get the value of the matrix
##set the value of the inverse
##get the value of the inverse
makeCacheMatrix <- function(x = matrix()) {
    ##Init inverse
    i<-NULL
    ##set matrix
    set <- function(y)
    {
        x<<-y
        i<<-NULL
    }
    ## get matrix
    get<- function()
    {
        x
    }
    ##set inverse
    setinverse<-function(inverse)
    {
        i<<-inverse
    }
    ##get inverse
    getinverse<-function()
    {
        i
    }
    ##return list of methods
    list(set=set,get=get,setinverse=setinverse,getinverse=getinverse)
}


## Write a short comment describing this function

##Calculate inverse of the special matrix that returns = makeCacheMatrix

cacheSolve <- function(x, ...) {
    ## Return a matrix that is the inverse of 'x'
    m<-x$getinverse()
    
    if(!is.null(m))
    {
        message("getting cached data")
        return(m)
    }
    data<-x$get()
    ## Calculating inverse with matrix multiplication
    m<-solve(data,...)
    ##Set inverse
    x$setinverse(m)
    ##return matrix
    m
}
