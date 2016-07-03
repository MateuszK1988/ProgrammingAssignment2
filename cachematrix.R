## caching inverse of matrix
## functions are saving procesor time caching and using cached inverted matrix
## 

## first function creates object which caches matrix  its inversion

makeCacheMatrix <- function(x = matrix()) {
        mi <- NULL
        set <- function(y) {
                x <<- y
                mi <<- NULL
        } 
        get <- function() x
        setinverse <- function(solve) mi <<- solve
        getinverse <- function() mi
        list(set = set, 
             get = get, 
             setinverse = setinverse,
             getinverse = getinverse)
}


## second function computes matrix inverion of matrix created with first function
## in case the matrix was inverted before and it's inversion is stored in cache function returns cache instead 
## of result of computing it's inversion.

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
        mi <- x$getinverse()
        if (!is.null(mi)) {
            message("getting cached data")
            return (mi)
        }
        mi <- x$get()
        mi <- solve(mi,...)
        x$setinverse(mi)
        mi
}


## example output:
## y <- makeCacheMatrix(matrix(1:4, 2, 2))
## cacheSolve(y)
##[,1] [,2]
##[1,]   -2  1.5
##[2,]    1 -0.5
## cacheSolve(y)
##getting cached data
##[,1] [,2]
##[1,]   -2  1.5
##[2,]    1 -0.5