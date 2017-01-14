## Put comments here that give an overall description of what your
## functions do

## There are four function this function creates
##  1. set: sets the new matrix and invalidates the matrix inverse
##  2. get: gets the current matrix
##  3. setinverse: sets the inverse of the given matrix
##  4. getinverse: gets the inverse matrix

makeCacheMatrix <- function(x = matrix()) {
    mi <- NULL
    
    set <- function(y){
        x <<- y
        mi <<- NULL
    }
    
    get <- function() x
    
    setinverse <- function(m) mi <<- m
    getinverse <- function() mi
    list(set = set, get = get, 
         setinverse = setinverse, getinverse = getinverse)

}


## create a cache for inverse of the matrix

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
    inversematrix <- x$getinverse()
    if(!is.null(inversematrix)){
        message("getting cached data")
        return(inversematrix)
    }
    
    matrix <- x$get()
    inversematrix <- solve(matrix, ...)
    x$setinverse(inversematrix)
    
    inversematrix
    
}
