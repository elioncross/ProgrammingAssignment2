## Coursera Course Title: R Programming
## Coursera Course ID: rprog-011
## Programming Assignment 2: Lexical Scoping
## Student: Ruben Leon

## Assigment Description: Matrix inversion is usually a costly computation 
## and there may be some benefit to caching the inverse of a matrix rather than 
## computing it repeatedly. 

## Assumption: it is assumed that the matrix supplied is always invertible.

## Below there is a pair of functions that cache the inverse of a matrix.

## This first function creates a special "matrix" object that can cache its inverse.
## It returns a special "vector", which is really a list containing a function to
##   1. set the matrix - a function called setM
##   2. get the matrix - a function called getM
##   3. set the inverse - a function called setinvM
##   4. get the inverse - a function called getinvM

makeCacheMatrix <- function(x = matrix()) {     
     
     invM = NULL
     
     setM = function(y) {
          x <<- y
          invM <<- NULL
     }
     getM = function() x
     setinvM = function(inverse) invM <<- inverse 
     getinvM = function() invM
     list(setM=setM, getM=getM, setinvM=setinvM, getinvM=getinvM)     
}

## This second function computes the inverse of the special "matrix" returned 
## by makeCacheMatrix above. If the inverse has already been calculated 
## (and the matrix has not changed), then cacheSolve should retrieve 
## the inverse from the cache.
## It returns a matrix that is the inverse of 'x'

cacheSolve <- function(x, ...) {
     mInv <- x$getinvM() 
     
     if(!is.null(mInv)) { 
          message("getting cached data")
          return(mInv) 
     }
     data <- x$getM() 
     mInv <- solve(data) 
     x$setinvM(mInv) 
     mInv      
}
