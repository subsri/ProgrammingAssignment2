## matrix object that can cache its inverse
makeCacheMatrix <- function( m = matrix() ) {

    ## Initializion
    i <- NULL

    ## set the matrix
    set <- function( matrix ) {
            m <<- matrix
            i <<- NULL
    }

    ## get the matrix
    get <- function() {
    	m
    }

    ## set the inverse of the matrix
    setInverse <- function(inverse) {
        i <<- inverse
    }

    ## get the inverse of the matrix
    getInverse <- function() {
        i
    }

    ## Return a list of the methods
    list(set = set, get = get,
         setInverse = setInverse,
         getInverse = getInverse)
}


## Compute the inverse of the special matrix returned by "makeCacheMatrix" . 
cacheSolve <- function(x, ...) {

    m <- x$getInverse()

    if( !is.null(m) ) {
            message("getting cached data")
            return(m)
    }

    data <- x$get()

    ## Calculates the inverse using matrix multiplication
    m <- solve(data) %*% data

    ## Sets the inverse to the object
    x$setInverse(m)

    m
}
