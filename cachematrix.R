## The functions caches the computation of matrix inverses, a potentially time-consuming process.

## makeCacheMatrix stores a list of functions designed to either set or return the value of both the matrix
## and the inverse.

makeCacheMatrix <- function(x = matrix()) {
        inv <- NULL
        set <- function(y) {
                x <<- y
                inv <<- NULL
        }
        get <- function() x
        setsolve <- function(solve) inv <<- solve
        getsolve <- function() inv
        list(set = set, get = get,
             setsolve = setsolve,
             getsolve = getsolve)
}


## The function cacheSolve is designed to verify if inv stored in getsolve is null; if not, it returns the 
## value stored in makeCacheMatrix. If null, the inverse of the matrix is computed via the function solve()
## and an inverse output is still returned.

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
        inv <- x$getsolve()
        if(!is.null(inv)) {
                message("getting cached data")
                return(inv)
        }
        data <- x$get()
        inv <- solve(data, ...)
        x$setsolve(inv)
        inv
}
