## Since matrix inverse computation is computationally expensive, it is a good idea to cache the
## inverse of a matrix (say, in a longer function) rather than to carry out massive computations
## repeatedly. The following functions create a special object that can store a matrix and cache the inverse
## of the matrix.

## makeCacheMatrix: 
## Objective - The purpose of this function is to create a special object that can cache the inverse
## of an input matrix.

makeCacheMatrix <- function(x = matrix()) {
        inv <- NULL
        set <- function(y) {
                x <<- y
                inv <<- NULL
        }
        get <- function() x
        setInverse <- function(inverse) inv <<- inverse
        getInverse <- function() inv
        list(set = set,
             get = get,
             setInverse = setInverse,
             getInverse = getInverse)
}
  


## cacheSolve:
## Objective - The purpose of this function is to compute the inverse of the object created in the makeCacheMatrix 
## function. In the event that the inverse has already been computed, the inverse should be retrieved from the cache.

cacheSolve <- function(x, ...) 
{
        ## Return a matrix that is the inverse of 'x'
inv <- x$getInverse()
        if (!is.null(inv)) {
                message("getting cached data")
                return(inv)
        }
        mat <- x$get()
        inv <- solve(mat, ...)
        x$setInverse(inv)
        inv
}

