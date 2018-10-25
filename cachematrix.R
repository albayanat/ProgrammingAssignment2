## Optimization of matrix inversion by caching the inverse of a matrix (supposed invertible)

## makeCacheMatrix enables the creation of special matrix that cache its inverse

makeCacheMatrix <- function(x = matrix()) {
        
        inv <- NULL
        set <- function(y) {
                x <<- y
                inv <<- NULL
        }
        get <- function() x
        setinv <- function(inverse) inv <<- inverse
        getinv <- function() inv
        list(set = set, get = get,
             setinv = setinv,
             getinv = getinv)
}


## cacheSolve enables to retreive the inverse of a (special) matrix using the cache. If not found in the cache, the inverse will be computed

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
        inv <- x$getinv()
        if(!is.null(inv)) {
                message("getting cached data")
                return(inv)
        }
        data <- x$get()
        inv<- solve(data, ...)
        x$setinv(inv)
        inv
}
