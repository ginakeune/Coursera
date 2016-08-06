## Problem statement from Assignment 2.

## Matrix inversion is usually a costly computation and there may be some benefit to caching the inverse of a matrix
## rather than compute it repeatedly. 

## Required to write a pair of functions that cache the inverse of a matrix.

## makeCacheMatrix - function that creates a special "matrix' object that can cache its inverse
## makeCacheMatrix creates a list containing functions to:
## 1. Set the value of the matrix
## 2. Get the value of the matrix
## 3. Set the value of the inverse of the matrix
## 4. Get the value of the inverse of the matrix

## makeCacheMatrix will be used by cacheSolve function to get or set the inverse of the matrix
## if it is in cache

makeCacheMatrix <- function(x = matrix()) {
        ## initialise to NULL
        cache_inv<-NULL
        
        ## create the matrix in the working environment
        set<-function(y){
                x<<-y
                cache_inv<<-NULL
        }
        
        ## This gets the value of the matrix
        get<-function() x
        
        ## This inverts the matrix and then stores in cache
        setmatrix<-function(solve) cache_inv<<- solve
        
        ## This gets the inverted matrix from cache
        getmatrix<-function() cache_inv
        
        ## Returns these functions to the working environment
        list(set=set, get=get,
             setmatrix=setmatrix,
             getmatrix=getmatrix)
}

## cacheSolve calculates the inverse of the matrix created in 
## makeCacheMatrix where if the inverted matrix does not exist in cache
## the matrix is then created in the working environment and the inverted
## value is stored in cache.
## If found in cache, it will prompt that it is getting it from cache

cacheSolve <- function(x, ...) {
        
        ## after being called from makeCacheMatrix, get the
        ## inverse stored in cache
        cache_inv <-x$getmatrix()
        
        ## check whether the inverted matrix from cache exists
        
        if(!is.null(cache_inv)){
                ## prompt that retrieving from cache
                message("getting cached data")
                
                ## return the matrix values to console
                return(cache_inv)
        }
        
        ## Else continue to create the matrix as it
        ## has not been cached
        datos<-x$get()
        
        ## Assumption is that the matrix is invertible
        ## amd invert matrix using solve() function
        cache_inv <-solve(datos, ...)
        
        ## Set the inverted matrix in cache
        x$setmatrix(cache_inv)
        
        ## Return the value in cache
        cache_inv
}

## Testing
## Initialise to create the functions
## a <- makeCacheMatrix()
##
## Create the matrix
## a$set(matrix(1:4,2,2))
##
## Check the values have been created in the working
## environment for the matrix
## a$get()
##      [,1] [,2]
## [1,]    1    3
## [2,]    2    4
## 
## 1st call to cacheSolve function, returns the inverse of the matrix
## cacheSolve(a)
##      [,1] [,2]
## [1,]   -2  1.5
## [2,]    1 -0.5
##
## 2nd call to cacheSolve function will prompt that it is
## getting it from cached data and returns the cached values
## cacheSolve(a)
##
## This was the message prompt that will indicate that it
## is retrieving from cache rather than storing as new values
##
## getting cached data
##      [,1] [,2]
## [1,]   -2  1.5
## [2,]    1 -0.5
