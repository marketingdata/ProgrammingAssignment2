##the makeCacheMatrix takes a matrix as an input and returns a list containing functions that will be used in the cacheSolve function to make the inverse of the matrix 

## makeCacheMatrix takes x as matrix and creates a list of functions which are set , get , setinv and getinv 

makeCacheMatrix <- function(x = matrix()) {
    inv <- NULL
    set <- function(y) {
        x <<- y
        inv <<- NULL
    }
    get <- function() x
    setinv <- function(invert) inv<<- solve(x)
    getinv <- function() inv
    list(set = set, get = get,
         setinv = setinv,
         getinv = getinv)

}


## cacheSolve function will use the functions listed above in the makeCacheMatrix to make the inverse of the matrix x

cacheSolve <- function(x, ...) {
    inv <- x$getinv()
    if(!is.null(inv)) {
        message("getting cached data")
        return(inv)
    }
    data <- x$get()
    inv <- solve(data, ...)
    x$setinv(inv)
    inv
}
