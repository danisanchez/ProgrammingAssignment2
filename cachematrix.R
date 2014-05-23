## Put comments here that give an overall description of what your
## functions do

## Write a short comment describing this function

makeCacheMatrix <- function(x = matrix()) {
        ## Set the value of the matrix
        i <- NULL
                set <- function(y) {
                x <<- y
                i <<- NULL
                }
        ## Get the value of the matrix
        get <- function() x
        
        ## Set the inverse of the matrix
        setinverse <- function(solve) i <<- solve
        getinverse <- function() i
        
        ## Get the inverse of the matrix
        list(set = set, get = get,
             setinverse = setinverse,
             getinverse = getinverse)
        }


## Write a short comment describing this function

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
        ## Get the inverse of the matrix
        i <- x$getinverse()
        
        ## Check if there is the matrix
        if(!is.null(i)) {
        message("Getting cached data")
        return(i)
        }
        ## If not get the inverse of the matrix
        data <- x$get()
        i <- solve(data, ...)
        ## Set the inverse of the matrix
        x$setinverse(i)
        i
}
