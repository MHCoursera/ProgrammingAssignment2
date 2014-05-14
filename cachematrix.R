## Below are two functions that can be sourced.  Combined, the two functions allow users
###     to use inverted matrices much more quickly than standard calucaltion of the inverted
###     matrix.  The functions do this by monitoring the existing matrix, and if it has not
###     changed, the inversion is recalled from a cache rather than recalculating it.
#
###     These functions are useful if matrix division (or other operations that require 
###     inversion) is performed repeatedly on more than one matrix.


## The first function is a housekeeping function that creates four shortcut calls for the 
## actual inversion process.  The four shortcuts are:
##              $set to establish the matrix
##              $get to recall the current matrix
##              $setinverse to calculate and establish the inverted matrix the first time
##              $getinverse to recall the inverted matrix if the original matrix has not changed.

makeCacheMatrix <- function(x = matrix()) {
        m <- NULL
        set <- function(y) {
                x <<- y
                m <<- NULL
        }
        get <- function() x
        setinverse <- function(solve) m <<- solve
        getinverse <- function() m
        list(set = set, get = get,
             setinverse = setinverse,
             getinverse = getinverse)

}


## The second function is the primary operative call to return the inverted matrix.
## This call ASSUMES an invertible matrix (e.g. sqaure).  If the matrix is unchanged
## it will return the inverted matrix from the cache via $getinverse; if the matrix has
## changed it will calculate and return the new inverted matrix, then store it in the 
## cache for future use.

cacheSolve <- function(x, ...) {
        m <- x$getinverse()
        if(!is.null(m)) {
                message("getting cached data")
                return(m)
        }
        data <- x$get()
        m <- solve(data, ...)
        x$setinverse(m)
        m
        }


#####  BELOW HERE NOT PART OF ASSIGNMENT - TEST CODE ONLY  #########

#a <- makeCacheMatrix()   # initialize
#class(a)
#class(a$set)
#a$set(matrix(1:4,2,2))         #set the matrix
#a$get                                        #get the matrix 
#cacheSolve(a)                        #calculate the mean 
#cacheSolve(a)                        #when is called back use the cached mean  
