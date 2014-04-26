## Since calculating the inverse of a matrix can be expensive,
## it's a good idea to be able to cache the inverse once it's been
## calculated, so it can be accessed multiple times without re-calculating.
## This set of functions allows us to calculate and cache the inverse of 
## a matrix. The matrix is assumed to always be invertible.


## makeCacheMatrix function:
## This function creates a special matrix object
## that can store its inverse.
## Example Usage: makeCacheMatrix(matrix(1:4,2)

makeCacheMatrix <- function(x = matrix()) {
        inv <- NULL
        set <- function(y) {
                x <<- y
                inv <<- NULL
        }
        get <- function() x
        setinv <- function(solve) inv <<- solve
        getinv <- function() inv
        list(set = set, get = get,
             setinv = setinv,
             getinv = getinv
}


## This function returns the inverse of an invertible matrix.
## If the inverse has been previously calculated, the function
## returns the cached result; otherwise it calculates the inverse
## and returns it.

cacheSolve <- function(x, ...) {
                
        ## try to get the cached version of the inverse
        inv <- x$getinv()
        
        ## if the inverse is not null, returned cached result
        if(!is.null(inv)) {
                message("Getting cached data")
                return(inv)
        }
        
        ## the inverse was null; calcualte the inverse
        data <- x$get()
        inv <- solve(data, ...)
        
        ## return the matrix that is the inverse of 'x'
        x$setinv(inv)
        inv
                
}
