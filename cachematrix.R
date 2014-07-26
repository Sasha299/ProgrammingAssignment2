## Function will cache the inverse of a matrix if it is not already cached

## makeCacheMatrix creates a special matrix

makeCacheMatrix <- function(x = matrix()) {
        ## initializes m and sets values of x and m 
        m <- NULL
        set <- function(y) {
                x <<- y
                m <<- NULL
        }
        get <- function() x
        ## inverse of the matrix
        setinverse <- function(solve) m <<- solve
        getinverse <<- function() m
        ## List of inverse values
        list(set = set, get = get,
             setinverse = setinverse,
             getinverse = getinverse)

}


## This function checks if there is a cached version.
## if there is a cached version it is returned
## if there isn't then it is calculated
## this function returns the inverse of X

cacheSolve <- function(x, ...) {
        ## setting value of M from function makeCacheMatrix
        m <- x$getinverse()
        ## if m is not empty then update with  value M from cache
        ## if it is then use solve function to find inverse
        
        if(!is.null(m)) {
                message("getting cached data")
                return(m)
        }
        data <- x$get()
        m <- solve(data)
        x$setinverse(m)
        m
}

