## The first function makeCacheMatrix takes a matrix as input and returns
## a special "matrix". That special" matrix" will be used by the function cacheSolve.
## The cacheSolve function takes a special "matrix" as input from the 'makeCacheMatrix'
## function and determines if the inverse of that matrix has been previously calculated.
## If it has been calculated, it will print a message indicating that the inverse is being
## pulled from the cache, otherwise it will calculate the inverse and return it.


 
makeCacheMatrix <- function(x = matrix()) {
## Takes a matrix as input and creates and returns a list of functions to be used
## in the cacheSolve function.
## 
## Returned Functions
##   getmat: Is set to the matrix that is passed into the function
##   setinv: Sets the inv value using <<-
##   getinv: Used to determine the innverse has been cached


        inv <- NULL
        getmat <- function() x
        setinv <- function(inverse) inv <<- inverse
        getinv <- function() inv
        list(getmat = getmat, setinv = setinv, getinv = getinv)
}


cacheSolve <- function(x, ...) {
## Takes the list of functions from makeCacheMatrix as input.  A test is performed to see
## if the inverse is NULL, if it is not the matrix is retrieved and its inverse is calculated
## and returned.  If the inverse in not NULL, a message is printed indicating that the inverse
## was previously run and the inverse is returned.

        
        mi <- x$getinv()
        if(!is.null(mi)){
                message("Retrieving Inverse From Cache")
                return(mi)
        }
        
        mat <- x$getmat()
        mi <- solve(mat, ...)
        x$setinv(mi)
        mi
}
