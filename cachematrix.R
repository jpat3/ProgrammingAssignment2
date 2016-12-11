## the two functions can be used to store and retrieve a matrix inverse

## This function creates a special "matrix" object that can cache its inverse

makeCacheMatrix <- function(x = matrix()) {
    
    
    #outputs a list of functions including:
    # 1. Set the value of the matrix, x
    # 2. Get the value of the matrix, x
    # 3. Set B, the inverse of the matrix, x
    # 4. Get B, the inverse of the matrix, x
    
    
    B <- NULL
    set <- function(y) {
        x <<- y
        B <<- NULL
    }
    get <- function() x
    setinv <- function(inverse_input) B <<- inverse_input
    getinv <- function() B
    list(set = set, get = get,
         setinv = setinv,
         getinv = getinv)

}


## This function computes the inverse of the special "matrix" returned by
## makeCacheMatrix above. If the inverse has already been calculated (and the
## matrix has not changed), then the cachesolve should retrieve the inverse from
## the cache.

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
    
    B <- x$getinv()
    if(!is.null(B)) {
        message("getting cached data")
        return(B)
    }
    A <- x$get()
    B <- solve(A)
    x$setinv(B)
    B
    
}
