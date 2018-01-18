## The cachesolve() function calculates the inverse of a matrix created in the makeCacheMatrix() function
## The makeCacheMatrix function stores a matrix 

## The makeCacheMatrix function creates a "special matrix" which is a list containing a function to
## set the value of the matrix, get the value of the matrix, set the value of the inverse, get the value of the inverse of the matrix

makeCacheMatrix <- function(x = matrix(nrow, ncol)) { ##specify that x is a matrix
        if(nrow(x) != ncol(x)){
                message("inverse not possible")
                return(NULL)                    ##only calculating the inverse for square matrix
        }
        i <- NULL                               ##i set to NULL initializing object in makeMatrix() to be used later in the function
        set <- function(y) {
                x <<- y                         ##Assign the unput argument to the x object in parent environment
                i <<- NULL                      ##Assign Null to i object in parent environment to clear any value of i that had been cached by a prior execution of cachematrixinverse()
        }
        get <- function() x                     ## get takes the value of x from the parent environment
        setinverse <- function(solve) i <<- solve ## i is defined in parent operator and we need to access it after setinverse() completes, use <<- to assign the input arg to the value of m in the parent environment
        getinverse <- function() i  ## retrieves i from parent environment
        list(set = set, get = get,
             setinverse = setinverse,
             getinverse = getinverse)           ##create list
}

## The cachesolve() function calculates the inverse of a matrix created in the makeCacheMatrix() function
##The cachesolve() function checks to see if the inverse has already been calculated. If it has already been calculated
## it gets the inverse from the cache and skips computation. Otherwise it calculates the inverse and sets 
## inverse of the data in the cache via the setinverse() function
## Write a short comment describing this function

cachesolve <- function(x, ...) {
        i <- x$getinverse()             ## retrieve the inverse from object passed as arg in cachematrixinverse()
        if(!is.null(i)) {               ## if there is an inverse from the object passed as an arg then the message and i will be returned
                message("getting cached data")
                return(i)
        }
        data <- x$get()                 ##takes x from object
        i <- solve(data, ...)           ##inverse calculated
        x$setinverse(i)
        i
}