## Put comments here that give an overall description of what your
## functions do

## makeCacheMatrix creates a list containing a function to set the matrxi,get 
## the matrix, set the inverse matrix, and get theinverse matrix 
makeCacheMatrix <- function(x = matrix()) {
    m <- NULL
    set <- function(y){
        x<<-y
        m<<-NULL
    }
    get <- function() x
    setinverse<- function(solve) m<<-solve
    getinverse<- function() m
    list(set=set,get=get,
         setinverse=setinverse,
         getinverse=getinverse)
}


## cacheSolve will calculates the inverse of an matrix,
## by firstly seeks the inverse otherwise it calculates
## the inverse and return the inverse

cacheSolve <- function(x, ...) {
        m<-x$getinverse()
        if(!is.null(m)){
            message("getting cached inverse matrix")
            return (m)
        }
        originalMatrix<-x$get()
        m<-solve(originalMatrix)
        x$setinverse(m)
        m
}
