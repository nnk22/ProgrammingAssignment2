## Cache the inverse of a matrix

## Creates a special matrix object that can cache its inverse
makeCacheMatrix <- function( m = matrix() ) {

    ## Initialize the inverse property
    x <- NULL

    ## Set the matrix
    set <- function( matrix ) {
            m <<- matrix
            x <<- NULL
    }

    ## Get the matrix
    get <- function() {
    	## Return the matrix
    	m
    }

    ## Set the inverse of the matrix
    setInverse <- function(inverse) {
        x <<- inverse
    }

    ## Get the inverse of the matrix
    getInverse <- function() {
        ## Return the inverse property
        x
    }

    ## Return a list of the methods
    list(set = set, get = get,
         setInverse = setInverse,
         getInverse = getInverse)
}


## Compute the inverse of the special matrix returned by "makeCacheMatrix" above. If the inverse has already been calculated 
##(and the matrix has not changed), then the "cachesolve" should retrieve the inverse from the cache.
cacheSolve <- function(y, ...) {

    ## Return a matrix that is the inverse of 'y'
    m <- y$getInverse()

    ## Return the inverse if it's already set
    if( !is.null(m) ) {
            message("getting cached data")
            return(m)
    }

    ## Get the matrix from our object
    data <- y$get()

    ## Calculate the inverse using matrix multiplication
    m <- solve(data) %*% data

    ## Set the inverse to the object
    y$setInverse(m)

    ## Return the matrix
    m
}
