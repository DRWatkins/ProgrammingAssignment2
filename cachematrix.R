## The following functions cache the data from a matrix inversion
## operation. The first function converts a matrix into an object
## that stores list of functions and values.
## The second function gives takes an input of the object described
## by the first function, then solves and stores the inverse.

## Name: makeCacheMatrix() [function]
## Creates an object of functions to store and access matrix data.
## Input: x, an invertible matrix
## Stored values: x, the original matrix input
##                xinv, the inverse of x [initialized to NULL]
## Functions: set(), sets values x and xinv (x,NULL)
##            get(), retrieves x
##            setinv(inv), sets values for xinv (inv)
##            getinv(), retrieves xinv
## Output: None

makeCacheMatrix <- function(x = matrix()) {

    xinv<-NULL
    set <- function(y){
        x <<- y
        xinv <<- NULL
    }
    get <- function(){
        x
    }
    setinv<-function(inv){
        xinv <<- inv
    }
    getinv<- function(){
        xinv
    }
    invisible(list(set = set, get = get, 
         setinv = setinv, getinv = getinv))
}


## Name: cacheSolve() [function]
## Checks for stored inverse of a matrix. If none exists, solves and stores.
## Inputs: x, a makeCacheMatrix object [described above]
## Action: Checks stored value of xinv in x. If xinv exists, outputs value.
##         Else solves for xinv, stores the value, then outputs it.
## Output: xinv

cacheSolve <- function(x) {
    xinv <- x$getinv()
    if (!is.null(xinv)) {
        print("Retrieving cached data")
        return(xinv)
    }
    data<-x$get()
    xinv<-solve(data)
    x$setinv(xinv)
    xinv
}
