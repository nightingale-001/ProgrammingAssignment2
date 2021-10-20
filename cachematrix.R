## Put comments here that give an overall description of what your
## functions do

## This function creates an object containing 4 functions:
## set (for vector value), get (for vector value), 
## setinv (for setting value of the inverse in the cache), and
## getinv(to get value of the inverse)

makeCacheMatrix <- function(x = matrix()) {
    inv <- NULL
    set <- function(y){
        x <<- y
        inv <<- NULL
    }
    get <- function (x)
    setinv <- function(inverse) inv <<- inverse
    getinv <- function() inv
    list(set=set,get=get,
         setinv=setinv,getinv=getinv)
}


## This function first tries to find the inverse from the cache with getinv.
## If it's there, it returns the cached value. Otherwise, it sets the value
## using solve() and setinv.

cacheSolve <- function(x, ...) {
    inv <- x$getinv()
    if(!is.null(inv)){
        message("getting cached inverse")
        return(inv)
    }
    inv <- solve(x$get())
    x$setinv(inv)
    inv
        ## Return a matrix that is the inverse of 'x'
}
