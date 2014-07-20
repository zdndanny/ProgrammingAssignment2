## Matrix inversion is usually a costly computation and their may be some 
## benefit to caching the inverse of a matrix rather than compute it repeatedly
## This file contains two functions that cache the inverse of a matrix.

## Examples:
## 
## > set.seed(1)
## > x <- makeCacheMatrix(matrix(rnorm(4),2,2))
## 
## > cacheSolve(x)
## [,1]       [,2]
## [1,] -1.885871 -0.9878433
## [2,]  0.217095  0.7405661
## 
## if x is not changed, next call will get cached result
## > cacheSolve(x)
## getting cached data
## [,1]       [,2]
## [1,] -1.885871 -0.9878433
## [2,]  0.217095  0.7405661
## 
## change x now
## > x <- makeCacheMatrix(matrix(runif(9),3,3))
## first call will resolv the matrix 
## > cacheSolve(x)
## [,1]      [,2]      [,3]
## [1,]  2.7566159  1.543352 -4.027603
## [2,]  0.5312684  2.674826 -2.425037
## [3,] -1.0755784 -1.874676  3.847517
## next call get cached result
## > cacheSolve(x)
## getting cached data
## [,1]      [,2]      [,3]
## [1,]  2.7566159  1.543352 -4.027603
## [2,]  0.5312684  2.674826 -2.425037
## [3,] -1.0755784 -1.874676  3.847517

## makeCacheMatrix: 
## This function creates a special "matrix" object 
## that can cache its inverse.

makeCacheMatrix <- function(x = matrix()) {
    inv <- NULL
    set <- function(y) {
        ## the <<- operator is used to assign a value to objects
        ## in an environment that is different from the current environment. 
        ## x is from the environment calling makeCacheMatrix
        ## inv is the environment 
        x <<- y
        inv <<- NULL
    }
    get <- function() x
    setinverse <- function(inverse) inv <<- inverse
    getinverse <- function() inv
    list(set = set, 
         get = get,
         setinverse = setinverse, 
         getinverse = getinverse)
}


## cacheSolve: This function computes the inverse of the special "matrix" 
## returned by makeCacheMatrix above. 
## If the inverse has already been calculated (and the matrix has not changed), 
## then the cachesolve should retrieve the inverse from the cache.

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
    
    inv <- x$getinverse()
    if(!is.null(inv)) {
        message("getting cached data")
        return(inv)
    }
    data <- x$get()
    inv <- solve(data, ...)
    x$setinverse(inv)
    inv
}
