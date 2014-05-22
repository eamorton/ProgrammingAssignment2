## These functions are to create a cache for calculating the inverse 
## of a matrix
## Programming assignment 2
## Script is templated on the "makeVector" and "cachemean" functions
## provided in the rdpeng example

## The first function creates a list of functions to take a square 
## matrix and cache its inverse once calculated

makeCacheMatrix <- function(x = matrix()) {
        m <- NULL
	  ## Sets default m to NULL
        set <- function(y) {
                x <<- y
                m <<- NULL
        }
        get <- function() x
	  ## The get function is the matrix itself
        setinverse <- function(solve) m <<- solve
	  ## The setinverse makes m the matrix inverse.  Getinverse returns m
        getinverse <- function() m
        list(set = set, get = get,
             setinverse = setinverse,
             getinverse = getinverse)
}

## The second function takes the output of the first function and checks 
## if the inverse has already been calculated.  If it has, it returns that
## cached inverse.  If it hasn't, it calculates the inverse, using the 
## "solve" function.  It returns the inverse.

cacheSolve <- function(x, ...) {
        ## Check if the cache already has an inverse for the matrix
        m <- x$getinverse()
        if(!is.null(m)) {
                message("Checking Cache")
                return(m)
        }
	  ## Now calculate the inverse and send it to the cache
        data <- x$get()
        m <- solve(data, ...)
        x$setinverse(m)
        m
}
