## cachematrix.R - two nested functions cache
## in memory the inverse of a matrix to
## reduce computational costs 

## Creates a "matrix" object than can cache its inverse
## Building a set of functions to set and get the value of a matrix ('x')
## and set and get the inverse of that matrix ('i')
## Returns a list describing these functions and data objects ('i' and 'x')
## to the parent environvironment

makeCacheMatrix <- function(x = matrix()) {
       ## starts by initializing matrices x (above) and i
        i <- NULL
        ## function to set matrix values
        set <- function(y) {
                x <<- y
                i <<- NULL
        }
        ## function to get matrix values
        get <- function() x
        ## function to set matrix inverse
        setinv <- function(inv) i <<- inv
        ## function to get matrix inverse
        getinv <- function() i
        ## returns list describing these functions
        list(set = set, get = get,
             setinv = setinv,
             getinv = getinv)
        
}

## Generates the inverse of the "matrix" created above
## Returns inverse stored in cache or newly calculated value 
## Avoids recalculation of inverse if matrix has not changed

cacheSolve <- function(x, ...) {
        ## calls to get matrix inverse of 'x' stored in cache
        i <- x$getinv()
        ## if 'i' is not equal to NULL, returns this cached value
        if(!is.null(i)) {
                message("getting cached data")
                return(i)
        }
        ## else if 'i' is equal to NULL, calculates, sets, and returns value
        data <- x$get()
        i <- solve(data,...)
        x$setinv(i)
        i
}

## Test code - check should be 2 by 2 Identity Matrix
## m1 <- matrix(c(1/2,-1/4,-1,3/4),nrow=2,ncol=2)
## myMatrix_object <- makeCacheMatrix(m1)
## im1 <- cacheSolve(myMatrix_object)
## check <- m1 %*% im1 