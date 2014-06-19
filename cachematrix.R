## Put comments here that give an overall description of what your
## functions do

## makeCacheMatrix returns a list of the following methods
##
## set resets the matrix "x" and also rests xinv to NULL since we expect the inverse will change
## get returns the matrix "x" in question
## setinv caches an inverse once calculated
## getinv returns the value cached in "xinv"

makeCacheMatrix <- function(x = matrix()) {
xinv <- NULL ##intially NULL since we don't know the inverse yet
        set <- function(y) {
                 x <<- y ##store this new value 
                 xinv <<- NULL  ## and reset the inverse 
         }
         get <- function() x
         setinv <- function(inverse) xinv <<- inverse #this is how the inverse is cached (as something other than NULL)
         getinv <- function() xinv #returns xinv whether NULL or not
         list(set = set, get = get, #Returns the list of functions
              setinv = setinv,
              getinv = getinv)
 }


## cacheSolve returns the inverse of a matrix
## Needs x to have the methods defined by makeCacheMatrix

cacheSolve <- function(x, ...) {
        xinv <- x$getinv() #first check if there is a cached inverse
        if(!is.null(xinv)) { ##There is a non-NULL value
                message("getting cached data") ##so we know that it was stored
                return(xinv) # return this inverse and halt
        }
        data <- x$get()  #otherwise, we have not calculated the inverse yet
        xinv <- solve(data, ...) #calculate the inverse
        x$setinv(xinv) #store this inverse for later
        xinv #return the inverse
}



