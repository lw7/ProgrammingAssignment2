## These functions can be used to cache the inverse of
## a large matrix so this computation doesn't have to 
## be done over and over again to save computing time.

## This function creates a matrix that holds in value of 
## its inverse

makeCacheMatrix <- function(x = matrix()) {
            m <- NULL
            set <- function(y) {
                    x <<- y
                    m <<- NULL
            }
            get <- function() x
            setinv <- function(solve) m <<- solve
            getinv <- function() m
            list(set = set, get = get,
                 setinv = setinv,
                 getinv = getinv)

}


## This function checks to see if the matrix inverse
## exists, if not, then computes the inverse using solve

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
            m <- x$getinv() 
            if(!is.null(m)) {
                    message("getting cached data")
                    return(m)
            }
            data <- x$get()
            m <- solve(data, ...)
            x$setinv(m)
            m

}
