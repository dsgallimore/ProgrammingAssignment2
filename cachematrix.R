## These functions take a Matrix and caches the inverse for future use to
## prevent the need to compute the inverse repeatedly. 


## Per assignment guidelines 
## this function assumes the matrix supplied is always invertible.


## This function creates a special matrix object that can cache it's inverse.

makeCacheMatrix <- function(x = matrix()) {
        m <- NULL
        set <- function(y) {
                x <<- y
                m <<- NULL
        }
        get <- function() x
        setinverse <- function(matrix) m <<- matrix
        getinverse <- function() m
        list(set = set, get = get,
             setinverse = setinverse, 
             getinverse = getinverse)
}


## This function computes the inverse of the special matrix returned in
## makeCacheMatrix and if not changed will retrieve the inverse.

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
        m <- x$getinverse()
        if(!is.null(m)) {
                message("getting cached data")
                return(m)
        }
        data <- x$get()
        m <- solve(data, ...)
        x$setinverse(m)
        m
}
