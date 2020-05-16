## Hello! The functions receive a matrix as argument and calculate it inverse 
## by the function solve(). If there is a stored matrix in cache it retrieves it
## instead of calculate it.


## This function receive a square matrix and defines the setters and getters 
##functions both for the matrix and the inverse making them available using a 
##list in the new MakeCacheMatrix element


makeCacheMatrix <- function(x = matrix()) {
        inv <- NULL
        set <- function(y){
                x <<- y
                inv <<- NULL
        }
        get <- function()x
        setinv <- function(inverse) inv <<- inverse
        getinv <- function() inv
        list (set=set, get=get, setinv=setinv, getinv=getinv)
 
}
## Retrieve the cache value for the matrix inverse, if exists it returns the
## value stored, if not it solve the matrix and save the inverse matrix in the
## cache variable

cacheSolve <- function(x, ...) {
        inv <- x$getinv()
        if(!is.null(inv)){
                message("getting cache data")
                return(inv)
        }
        data <- x$get()
        inv <- solve(data, ...)
        x$setinv(inv)
        inv
}
