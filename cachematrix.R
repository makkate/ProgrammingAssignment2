
## makeCacheMatrix creates a special matrix object, and then cacheSolve 
## calculates the inverse of the matrix.
## If the matrix inverse has already been calculated, it will instead 
## find it in the cache and return it, and not calculate it again.


makeCacheMatrix <- function(a=matrix()) {
    inv_a <- NULL
    set <- function(b) {
        a <<- b
        inv_a <<- NULL
    }
    get <- function() a
    setinv <- function(inv) inv_a <<- inv
    getinv <- function() inv_a
    list(set=set, get = get,setinv = setinv, getinv = getinv)
}



## The function cacheSolve returns the inverse of a matrix A created with
## the makeCacheMatrix function.
## If the cached inverse is available, cacheSolve retrieves it, while if
## not, it computes, caches, and returns it.


cacheSolve <- function(a,...) {
    inv_a <- a$getinv()
    if(!is.null(inv_a)) {
        message("getting cached inverse matrix")
        return(inv_a)
    }
    data <- a$get()
    inv_a <- solve(data)
    a$setinv(inv_a)
    inv_a
}
