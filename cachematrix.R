## Functions 'makeCacheMatrix' and 'cacheSolve' handle caching and retrieving of inverted matrices

## 'makeCacheMatrix' function creates a matrix containind a list with 4 functions:
## 1. set: creates a matrix
## 2. get: retrieves a matrix created with 'set' function
## 3. setinv: creates an inverse of a matrix
## 4. getinv: retrieves an inverse of a matrix created with 'setinv' function


makeCacheMatrix <- function(x = matrix()) {
        I <- NULL
        set <- function(y) {
                x <<- y
                I <<- NULL
        }
        get <- function() x
        setinv <- function(inv) I <<- inv
        getinv <- function() I
        list(set = set, get = get,
             setinv = setinv,
             getinv = getinv)
}


## Function 'cacheSolve' calculates an inverse of the invertable "matrix" created with function 'makeCacheMatrix'
## Whenever the inverse has already been calculated, the inverse is pulled from cache and calculation skipped
## In case an inverse cannot be found from cache, the function calculates an inverse and stores it in cache.

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
        I <- x$getinv()
        if(!is.null(I)) {
                message("getting cached data")
                return(I)
        }
        data <- x$get()
        I <- solve(data, ...)
        x$setinv(I)
        I
}
