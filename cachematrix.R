## The assignment was to write two functions to cache the inverse of a matrix(makeCacheMatrix)
## and one to invert a matrix unless it has already been done (cacheSolve).  if is has already 
## been solvved, cacheSolve should retrieve the already inverted matrix.

## makeCacheMatrix does the following:
##      1. sets the value of the matrix
##      2. gets the value of the matrix
##      1. sets the inverse of the matrix
##      1. gets the inverse of the matrix
## it is the same function as the example given for the vector modified for a matrix


## makeCacheMatrix checks for an already calculated matrix inverse
## if it finds one it retreives it.  If not, it calculates it.
## again, it is the same function as the example given for the vector modified for a matrix

makeCacheMatrix <- function(x = matrix()) {
        s <- NULL
        set <- function(y) {
                x <<- y
                s <<- NULL
        }
        get <- function() x
        setInv <- function(solve) s <<- solve
        getInv <- function() s
        list(set = set, get = get,
             setInv = setInv,
             getInv = getInv)

}


## cacheSolve calculates the inverse of an invertable matrix created in the above function
## unless it has already been inverted.  If it has, cacheSolve retrieves that solution.

cacheSolve <- function(x, ...) {
        s <- x$getInv()
        if(!is.null(s)) {
                message("getting cached data")
                return(s)
        }
        data <- x$get()
        s <- solve(data, ...)
        x$setInv(s)
        s
}
