## Assignments Part2 for R-Programming Week3
## @written by Youngok Kim, joylife052@gmail.com


## makeCacheMatrix creates a special "matrix", which is really a list containing
## a function to
##  1. set the value of the matrix
##  2. get the value of the matrix
##  3. set the value of the inverse matrix
##  4. get the value of the inverse matrix

makeCacheMatrix <- function(x = matrix()) {
    m <- NULL
    set <- function(y) {
        x <<- y
        m <<- NULL
    }
    get <- function() x
    setsolve <- function(inv) m <<- solve
    getsolve <- function() m
    list(set = set, get = get,
         setsolve = setsolve,
         getsolve = getsolve)
}


## cacheSolve calculates the inverse matrix of the special "matrix" created 
## with the makeCacheMatrix() function. However, it first checks to see if the
## inverse matrix has already been calculated. If so, it gets the inverse matrix
## from the cache and skips the computation. Otherwise, it checks data is square
## matrix and inverse exists. if so, it calcualtes the inverse matrix of data 
## and sets the value of the inverse matrix in the cache via the setsolve() 
## function.

cacheSolve <- function(x, ...) {
    ## Return a matrix that is the inverse of 'x'
    m <- x$getsolve()
    if(!is.null(m)) {
        message("getting cached data")
        return(m)
    }
    data <- x$get()
    
    if(nrow(data) == ncol(data)) {  # check square matrix, or not
        if(det(data) != 0) {        # if exist inverse matrix
            m <- solve(data, ...)
            x$setsolve(m)
        } else{
            message("does not exist inverse matrix")
        }
    } else {
        message("not square matrix")
    }
    m
}
