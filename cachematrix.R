## Calculation of the Inverse of a Matrix,
## but returning a cached result if the Inverse of this Matrix has already been calculated earlier

## Usage
## y <- makeCacheMatrix(x = matrix())
## z1 <- cacheSolve(y)
## z2 <- cacheSolve(y)

## makeCacheMatrix(x = matrix())
## Construct the cached version of matrix x

makeCacheMatrix <- function(x = matrix()) {
    m <- NULL
    set <- function(y) {
        x <<- y
        m <<- NULL
    }
    get <- function() x
    setsolve <- function(solve) m <<- solve
    getsolve <- function() m
    list(set = set, get = get,
         setsolve = setsolve,
         getsolve = getsolve)
    
}


## cacheSolve(y)
## y is a matrix constructed by the function makeCacheMatrix
## the result is either calculated or returned from cache if available

cacheSolve <- function(x, ...) {
    m <- x$getsolve()
    if(!is.null(m)) {
        message("getting cached data")
        return(m)
    }
    data <- x$get()
    m <- solve(data, ...)
    x$setsolve(m)
    m
}