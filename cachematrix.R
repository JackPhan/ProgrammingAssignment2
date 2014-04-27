## The first function, makeCacheMatrix creates a special "matrix", 
## which is really a list containing a function to
##     set the value of the matrix
##     get the value of the matrix
##     set the inverse of the matrix
##     get the inverse of the matrix


## Write a short comment describing this function

makeCacheMatrix <- function(x = matrix()) {
        m <- NULL
        set <- function(y) {
                x <<- y
                m <<- NULL
        }
        get <- function() x
        setInverse <- function(solve) m <<- solve
        getInverse <- function() m
        list(set = set, get = get,
             setInverse = setInverse,
             getInverse = getInverse)
}

## The following function inverses the special "matrix" created with above function. 
## However, it first checks to see if the matrix has already been inversed. 
## If so, it gets the inverse from the cache and skips the inverse. 
## Otherwise, it inverse the matrix and sets the inverse of the matrix in 
## the cache via the setInverse function.

cacheSolve <- function(x, ...) {
        m <- x$getInverse()
        if(!is.null(m)) {
                message("getting cached data")
                return(m)
        }
        data <- x$get()
        m <- solve(data, ...)
        x$setInverse(m)
        m
}
