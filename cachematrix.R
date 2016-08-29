## The functions below calculates the inverse of a matrix and caches 
## to parent environment.  If no changes the function will get the 
## value from cache instead of repeating the calculation.

## makeCacheMatrix creates a "special" matrix object that can cache its inverse

makeCacheMatrix <- function(x = matrix()) {
        m = NULL                           # m undefined
        setMatrix <- function(y) {         # set "caches" x to parent environment
                x <<- y
                m <<- NULL
        }
        getMatrix <- function() x          # get reads x from "cache"
        setinv <- function(inv) m <<- inv  # setinv "caches" m to parent env.
        getinv <- function() m             # getinv reads m from "cache"
        list(setMatrix = setMatrix, 
             getMatrix = getMatrix,        
             setinv = setinv,
             getinv = getinv)
}


## cacheSolve computes the inverse of the inverse of the "special" matrix returned
## by makeCacheMatrix above.  If the inverse has already been calculated (and the
## matrix has not changed), then cacheSolcve will retrieve the inverse from the
## cache.

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
        m <- x$getinv()
        if(!is.null(m)) {
                message("getting cached data")
                return(m)
        }
        data <- x$getMatrix()
        m <- solve(data, ...)
        x$setinv(m)
        m
}
