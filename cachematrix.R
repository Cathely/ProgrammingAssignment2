## Put comments here that give an overall description of what your
## functions do
##  a pair of functions that cache the inverse of a matrix.


## For example:
# x <-matrix(c(1,-1,1,1,2,1,2,0,3),3,3)
# > x
#       [,1] [,2] [,3]
# [1,]    1    1    2
# [2,]   -1    2    0
# [3,]    1    1    3
# This is the inverse of the object x:
# solve(x)
#        [,1]    [,2]     [,3]
# [1,]    2 -0.3333333 -1.3333333
# [2,]    1  0.3333333 -0.6666667
# [3,]   -1  0.0000000  1.0000000


## input :
# >x <-matrix(c(1,-1,1,1,2,1,2,0,3),3,3)
# >source("cachematrix.R")
# >n <- makeCacheMatrix(x)

# output:
# > cacheSolve(n)
#        [,1]    [,2]    [,3]
# [1,]    2 -0.3333333 -1.3333333
# [2,]    1  0.3333333 -0.6666667
# [3,]   -1  0.0000000  1.0000000

# repeat output:
# > cacheSolve(n)
# getting cached data
# [,1]       [,2]       [,3]
# [1,]    2 -0.3333333 -1.3333333
# [2,]    1  0.3333333 -0.6666667
# [3,]   -1  0.0000000  1.0000000

## Write a short comment describing this function
## This function creates a special "matrix" object that can cache its inverse.
makeCacheMatrix <- function(x = matrix()) {
        m <- NULL
        set <- function(y){
        ## << can be used to assign a value to an object in a "bigger" environment 
                x <<- y
                m <<- NULL
        }
        get <- function() x
        setinverse <- function(solve) m <<- solve
        getinverse <- function() m
        list(set = set, get =get,
             setinverse = setinverse,
             getinverse = getinverse)
}


## Write a short comment describing this function
## This function computes the inverse of the special "matrix" returned by makeCacheMatrix above. 
## If the inverse has already been calculated (and the matrix has not changed), then the cachesolve should 
## retrieve the inverse from the cache.
cacheSolve <- function(x, ...) {
        ## this x means the return value of makeCacheMatrix(),so it is a list.
        m <- x$getinverse()
        ## if m has the not null value, it means that m has already had the inverse of x.
        ## So don't have to compute it repeatedly.
        if(!is.null(m)){
                message("getting cached data")
        ## Return a matrix that is the inverse of 'x'
                return(m)
        }
        ## otherwise, get the matrix and compute the inverse. 
        data <- x$get()
        m <- solve(data,...)
        x$setinverse(m)
        ## Return a matrix that is the inverse of 'x'
        m
}
