###############################################################################
## File         : cachematrix.R
## Revision     : 0.1
## Date         : 19-Jan-2015
## Author       : chris.maselli+coursera@gmail.com
## Description  : Programming Assignment 2
##                https://class.coursera.org/rprog-010/human_grading/view/courses/973491/assessments/3/submissions
##
##                Assignment: Caching the Inverse of a Matrix
##                Matrix inversion is usually a costly computation and there
##                may be some benefit to caching the inverse of a matrix rather
##                than computing it repeatedly (there are also alternatives to
##                matrix inversion that we will not discuss here). Your
##                assignment is to write a pair of functions that cache the
##                inverse of a matrix.
###############################################################################

## makeCacheMatrix: creates a special "matrix" object that can cache its
## inverse. The special matrix returned is really a list containing functions
## that perform the following operations:
## 1.  set the value of the matrix
## 2.  get the value of the matrix
## 3.  set the value of the inverse
## 4.  get the value of the inverse

makeCacheMatrix <- function(x = matrix()) {
    i <- NULL
    set <- function(y) {
        x <<- y
        i <<- NULL
    }
    get <- function() x
    setinverse <- function(inverse) i <<- inverse
    getinverse <- function() i
    list(set = set, get = get,
         setinverse = setinverse,
         getinverse = getinverse)
}

###############################################################################

## cacheSolve: This function computes the inverse of the special "matrix"
## returned by makeCacheMatrix above. If the inverse has already been
## calculated (and the matrix has not changed), then cacheSolve should
## retrieve the inverse from the cache.
## This function assume that the matrix supplied is always invertible.

cacheSolve <- function(x, ...) {
    ## Return a matrix that is the inverse of 'x'
    i <- x$getinverse()
    if(!is.null(i)) {
        message("getting cached data")
        return(i)
    }
    data <- x$get()
    i <- solve(data, ...)
    x$setinverse(i)
    i
}

###############################################################################

## Test for the functions in this module.

test <- function(x, ...) {

    cacheMatrix <- makeCacheMatrix()
    
    result      <- cacheSolve(cacheMatrix)
    
    cacheMatrix
}

###############################################################################
