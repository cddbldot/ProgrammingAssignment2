## makeCacheMatrix: returns an object with the ability to store
## a cached inverse of a given matrix
##
## cacheSolve: calculates the inverse of the matrix and stores it
## within the makeCacheMatrix-Object to retrieve at a later stage
##
## Usage:
##
## m <- matrix(c(1, 2, 3, 4), 2, 2)
## cm <- makeCacheMatrix(m)
## cacheSolve(cm) # computes the inverse
## cacheSolve(cm) # NO computation, returns just the cached result


## Returns an object of type "makeCacheMatrix"
makeCacheMatrix <- function(x = matrix()) {
    
    ## variable to store the inverse of the matrix
    inverse <- NULL
    
    ## reset the object, if set-method is invoked
    set <- function(y) {
        x <<- y
        inverse <<- NULL
    }
    
    ## return the matrix
    get <- function() x
    
    ## set the inverse of the matrix
    setinv <- function(inv) inverse <<- inv
    
    ## get the inverse of the matrix
    getinv <- function() inverse
    
    ## return a list of all methods
    list(
        set = set,
        get = get,
        setinv = setinv,
        getinv = getinv
    )
}


## Returns a matrix that is the inverse of 'x'
## x has to be an object of type "makeCacheMatrix".
cacheSolve <- function(x, ...) {

    ## Checks if there is already a cached inverse available
    ## within x, returns the value and stops executing the function.
    inv <- x$getinv()
    if(!is.null(inv)) {
        message("getting cached data")
        return(inv)
    }
    
    ## If the inverse stored in x is null, the following code
    ## gets the matrix, computes the inverse, stores the
    ## result in x an returns the inverse.
    data <- x$get()
    inv <- solve(data)
    x$setinv(inv)
    inv
}
