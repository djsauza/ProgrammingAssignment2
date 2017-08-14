## This assignment contains two functions, which are written to avoid the repeated calculation of inverse matrices, which
## are costly computationally. 

##The first function, makeCacheMatrix, creates a special "matrix" object that can cache its inverse.

makeCacheMatrix <- function(x = matrix()) {
        m <- NULL
        set <- function(y) {
                x <<- y
                m <<- NULL
                
        }
        get <- function() x
        setmatrix <- function(solve) m <<- solve
        getmatrix <- function() m
        list(set = set, get = get,
             setmatrix = setmatrix,
             getmatrix = getmatrix)
}

## The second, cachesolve, first checks whether the inverse matrix has already been calculated
## If the inverse has already been calculated, it returns the already-calculated inverse matrix.
## If, however, the inverse has not yet been calculated, cachesolve returns the inverse of the new matrix. 

cachesolve <- function(x, ...) {
        m <- x$getmatrix()
        if(!is.null(m)) {
                message("getting cached data")
                return(m)
        }
        data <- x$get()
        m <- solve(data, ...)
        x$setmatrix(m)
        m
}
        ## Return a matrix that is the inverse of 'x'

