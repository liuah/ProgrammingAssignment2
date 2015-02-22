
## Creates and returns a CacheMatrix that contains the get and 
## set functions for the input matrix and its inverse

makeCacheMatrix <- function(input.matrix=matrix()){
    inverse.matrix <- NULL
    set <- function(y){
        input.matrix <<- y
        inverse.matrix <<- NULL
    }
    get <- function() input.matrix
    setmatrix <- function(computed.inverse.matrix) inverse.matrix <<- computed.inverse.matrix
    getmatrix <- function() inverse.matrix
    list(set=set,get=get,setmatrix=setmatrix,getmatrix=getmatrix)
}

## Takes a CacheMatrix and prints the message "getting cached data" and 
## returns the cached inverse matrix if its inverse has been cached, 
## otherwise computes the inverse matrix, saves it in the cache, 
## and returns the inverse matrix

cacheSolve <- function(input.matrix,...){
    inverse.matrix <- input.matrix$getmatrix()
    if(!is.null(inverse.matrix)){
        message("getting cached data")
        return(inverse.matrix)
    }
    data <- input.matrix$get()
    inverse.matrix <- solve(data,...)
    input.matrix$setmatrix(inverse.matrix)
    inverse.matrix
}
