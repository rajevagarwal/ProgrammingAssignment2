## Functions for caching the inverse of a matrix. It is assumed the matrix is
## always invertable, and a square matrix.
##
## Function makeCacheMatrix() caches matrix (x) and its inverse (m), and returns 
## a list of get and set functions helpful to access these cacahed variables, 
## matequal function tests that the matrix has not changed. 
##
## Function cacheSolve() accepts the special vector (x) created by makeCacheMatrix()
## and computes the inverse of the square matrix. If the matrix has not changed
## it returns the cached inverse, otherwise it will calculate the matrix inverse

## makeCacheMatrix(x) creates a special matrix object that can cache its inverse
makeCacheMatrix <- function(x = matrix()) {
        
        # inverse of matrix
        m <- NULL
        
        # setter for matrix x
        set <- function(y) {
                x <<- y
                m <<- NULL
        }
        
        # getter for matix x
        get <- function() {
                x
        }
        
        # setter for caching inverse m
        setinverse <- function(inv) {
                m <<- inv
        }
        
        # getter for inverse m
        getinverse <- function() {
                m
        }
        
        # test equality of matrix x and other matrix
        matequal <- function (other) {
                is.matrix(other) && dim(x) == dim(other) && all(x == other)
        }
        
        list(set = set, get = get,
             setinverse = setinverse,
             getinverse = getinverse, matequal = matequal)
}


## cacheSolve() computes the inverse of special matrix returned by makeCacheMatrix
cacheSolve <- function(x, ...) {

        # get the inverse
        m <- x$getinverse()
        
        # return inverse if it exists and matrix has not changed
        if(!is.null(m) && x$matequal(solve(m))) {
                message("getting cached data")
                return(m)
        }
        
        # get current matrix and find its inverse
        data <- x$get()
        m <- solve(data, ...)
        
        # set the new matrix and data
        x$set(data)
        x$setinverse(m)
        
        # return the inverse
        m
}
