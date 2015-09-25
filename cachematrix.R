## these functions combine to allow the use of a new "matrix" that
## caches its inverse if it has been previously calculated and another
## function that caches and returns the result of the inversion
##
## invertible matrices are handled correctly

## makeCacheMatrix creates a special "matrix" that is really a list
## containing functions that:
## 1. set the value of the matrix (set)
## 2. get the value of the matrix (get)
## 3. set the inverse of the matrix (setinverse)
## 4. get the inverse of the matrix (getinverse)

makeCacheMatrix <- function(x = matrix()) {
    m <- NULL
    set <- function(y) {
        x <<- y
        m <<- NULL
    }
    get <- function() x
    setinverse <- function(inverse) m <<- inverse
    getinverse <- function() m
    list(set = set, get = get,
         setinverse = setinverse,
         getinverse = getinverse)
}


## The following function calculates the inverse of the special
## "matrix" created with the above function. It first checks to see if
## the inverse is cached, otherwise it creates and caches the inverse
##
## if the matrix is not invertible it prints a message and returns NaN
## For non-invertible matrices NaN is cached

cacheSolve <- function(x, ...) {
    ## Return a matrix that is the inverse of 'x'
    m <- x$getinverse()
    if (!is.null(m)) {
        message("getting cached data")
        if (!is.matrix(m) && is.na(m)) {
            message("matrix is not invertible")
            return(NaN)
        }
        return(m)
    }
    data <- x$get()
    if (det(data) == 0) {
        message("matrix is not invertible")
        m <- NaN
        x$setinverse(m)
        return(m)
    }
    m <- solve(data)
    x$setinverse(m)
    m
}
