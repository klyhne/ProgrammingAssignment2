## Functions to make af matrix capable of caching its inverse
## and a function to return the cached inverse or cache the inverse if not yet calculated

## Creates a list of functions to set and get the value of a matrix 
## as well as the inverse of the same matrix

makeCacheMatrix <- function(x = matrix()) {
        inverse <- NULL
        set <- function(y) {
                x <<- y
                inverse <<- NULL
        }
        get <- function() x
        setinverse <- function(solve) inverse <<- solve
        getinverse <- function() inverse
        list(set = set, get = get,
             setinverse = setinverse,
             getinverse = getinverse)
}


## Returns a matrix that is the inverse of 'x'
## Returns cached inverse if available
## Calculates, stores and returns inverse if no inverse is cached

cacheSolve <- function(x, ...) {
        inverse <- x$getinverse()
        if(!is.null(inverse)) {
                message("getting cached matrix")
                return(inverse)
        }
        matrix <- x$get()
        inverse <- solve(matrix, ...)
        x$setinverse(inverse)
        inverse
}
