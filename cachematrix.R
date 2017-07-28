## Put comments here that give an overall description of what your
## functions do

## Write a short comment describing this function

# Assignments Part2 for R-Programming Week3
# @written by Youngok Kim, joylife052@gmail.com
#


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


## Write a short comment describing this function

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
    m <- x$getsolve()
    if(!is.null(m)) {
        message("getting cached data")
        return(m)
    }
    data <- x$get()
    
    if(nrow(data) == ncol(data)) { # check square matrix, or not
        if(det(data) != 0) { # if exist inverse matrix
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
