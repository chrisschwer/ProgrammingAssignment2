## Put comments here that give an overall description of what your
## functions do

## MakeCacheMatrix creates a Matrix object, that can be manipulated with the following commands:
## 1. set: sets the value
## 2. get: gets the value
## 3. setInverse: sets the inverse
## 4. getInverse: gets the inverse

makeCacheMatrix <- function(x = matrix()) {

        inv <- NULL  # set the cache `inv` to NULL
        set <- function (y) {
                x <<- y  # assign y to the matrix value
                inv <<- NULL  # (re)set the cache to NULL
        }
        get <- function () x  # return the current matrix value
        setInverse <- function(solve) {inv <<- solve}  # set the inverse
        getInverse <- function () {inv}  # return the inverse
        list (set = set, get = get,
              setInverse = setInverse,
              getInverse = getInverse)  # Create the list for the matrix object        
}



## This function reads out the cache, if it is not empty, it will return the cache,
## else it will calculate the value.

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
        
        inv <- x$getInverse()  # Read the cache
        if (!is.null(inv)) {
                return(inv)
        }  #check if the cache is not empty. If it is not, return the cached value.
        data <- x$get()  # read the matrix
        inv <- solve(data, ...)  # calculate the inverse
        x$setInverse(inv)  # store the inverse in the object
        inv  # return the calculated inverse        
}
