## From the original programming assignement:
## This function creates a special "matrix" object that can cache its inverse.

makeCacheMatrix <- function(x = matrix()) {
        
        ## start with a null vector
        m <- NULL
        
        ## create the matrix, using the same type of process in makeVector
        set <- function(y) {
          x<<-y
          m<<-NULL
        }
        
        get <- function() x
        
        setmatrix <- function(solve) m <<- solve
        
        getmatrix <- function() m
        
        list(set=set, get=get, setmatrix=setmatrix, getmatrix=getmatrix)
}


## From the original programming assigment:
## "This function computes the inverse of the special "matrix" returned by 
## makeCacheMatrix above. If the inverse has already been calculated (and 
## the matrix has not changed), then the cachesolve should retrieve the 
## inverse from the cache."

cacheSolve <- function(x, ...) {
        
        ## call getmatrix to get the matrix
        m <- x$getmatrix()
        
        ## if it's not null, return the matrix
        if(!is.null(m)){
                message("getting cached data")
                return(m)
        }

        matrix <- x$get()

        ## solve for the matrix        
        m <- solve(matrix, ...)
        
        ## set it in the cache.
        x$setmatrix(m)
        
        ## return the solved matrix
        m
}
