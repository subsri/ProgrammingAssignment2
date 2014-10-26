## A pair of functions that cache the inverse of a matrix

## Creates a special matrix object that can cache its inverse
makeCacheMatrix <- function( m = matrix() ) {

	## Initializes the inverse property
    i <- NULL

    ## Setss the matrix
    set <- function( matrix ) {
            m <<- matrix
            i <<- NULL
    }

    ## Gets the matrix
    get <- function() {
    	## Retursn the matrix
    	m
    }

    ## Sets the inverse of the matrix
    setInverse <- function(inverse) {
        i <<- inverse
    }

    ## Gets the inverse of the matrix
    getInverse <- function() {
        ## Return the inverse property
        i
    }

    ## Returns a list of the methods
    list(set = set, get = get,
         setInverse = setInverse,
         getInverse = getInverse)
}


## Compute the inverse of the special matrix returned by "makeCacheMatrix". 
## If the inverse has already been calculated then the "cachesolve" retrieves the inverse from the cache.
cacheSolve <- function(x, ...) {

    ## Returns a matrix that is the inverse of 'x'
    m <- x$getInverse()

    ## Returns the inverse if its already set
    if( !is.null(m) ) {
            message("getting cached data")
            return(m)
    }

    ## Gets the matrix from object
    data <- x$get()

    ## Calculates the inverse using matrix multiplication
    m <- solve(data) %*% data

    ## Sets the inverse to the object
    x$setInverse(m)

    ## Returns the matrix
    m
}
