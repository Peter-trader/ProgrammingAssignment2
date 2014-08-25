## This script contains two functions which together enable caching of a 
## previously derrived matrix inverse. The function makeCacheMatrix is
## a constructor. makeCacheMatrix is used to create a cacheable matrix object
## which is operated on by the cacheSolve function.

# Returns a list of methods for accessing and modifying a cached matrix and its
# inverse. Data objects are defined in the scope of this function (a sub 
# environment of .GlobalEnv) via formal arguments. To construct a cacheble
# matrix, assign the output of makeCacheMatrix() to a global variable, and 
# use the '$' operator to invoke methods. For example 'foo$get()'

makeCacheMatrix <- function(x = matrix(), inv = NULL) {
        set <-function(y){
                #Could check for 'squareness' here if desired
                x <<- y
                inv <<- NULL
        }
        get <- function() x
        setInverse <- function(mInv) inv <<- mInv
        getInverse <- function() inv
        list(set=set, get=get, setInverse=setInverse, getInverse=getInverse)
        
}


## For a given square, invertible matrix cacheSolve returns its inverse either
## by retrieving a previously cached solution, or computing it directly. It
## can read a cacheble matrix created with the makeCacheMatrix, which is passed
## by argument.

cacheSolve <- function(x, ...) {
        inv <- x$getInverse()
        if(!is.null(inv)){
                message("getting cached data")
                return(inv)
        }
        mat <- x$get()
        inv <- solve(mat, ...)
        x$setInverse(inv)
        inv
        
}

## The code below is used to test functionality. 
foo <-makeCacheMatrix()
bar <- matrix(c(1,2,2,5,2,3,3,3,4), nrow=3, ncol=3)

foo$set(bar)
cacheSolve(foo)

# Inspect environments:
ls()
ls(environment(foo$set))
get(envir=environment(foo$set), "x")
get(envir=environment(foo$set), "inv")