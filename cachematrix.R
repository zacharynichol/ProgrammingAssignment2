## Collection of functions in assignment #2 for working with matrixes. 
## R-Programming Course - Week 3
## Z. Nichol - Nov. 22, 2014 - Hamilton, Ontario, Canada


## makeCacheMatrix: This function creates a special "matrix" object that can 
## cache its inverse.
makeCacheMatrix <- function(x = matrix()) {

        ## m is the new solved matrix, inital value is null
        m <- NULL
        
        ## set: takes the supplied/starting matrix and loads it into x for storage. 
        ## this is also resets m to null, if a new matrix was cached, so that when getSolved is called
        ## again, the logic will know that a new solved matrix needs to be built/rebuilt. 
        set <- function(y) {
                x <<- y
                m <<- NULL
        }
        
        ## get: returns the value of the original matrix
        get <- function() x
        
        ## setsolve: when called from cacheSolve, this function solves and returns the new matrix.
        setsolve <- function(solve) m <<- solve
        
        ## getsolve: when called from cacheSolve, this function simply returns the preSolved matrix. 
        getsolve <- function() m
        
        ## this code is run at the end of the makeCacheMatrix and builds a list which contains the functions and data. 
        list(set = set, get = get,
             setsolve = setsolve,
             getsolve = getsolve)        
}


## cacheSolve: This function computes the inverse of the special "matrix" 
## returned by makeCacheMatrix above. If the inverse has already been calculated
## (and the matrix has not changed), then the cachesolve should retrieve the 
## inverse from the cache.
cacheSolve <- function(x, ...) {
        
        ## into m, load whatever value is stored in getSolve(m in the above funciton)
        m <- x$getsolve()
        
        ## then test to see if this value is m (or to see if this matrix has previously been solved)
        if(!is.null(m)) {
                ## if not null / previously solved, then simlple return the value. 
                ## the return command ends the function within this if statement and stops the other code from runnign. 
                message("getting cached data")
                return(m)
        }
        
        ## if the above if dosn't stop the code then run these lines:
        ## load the data from the list object, solve and then store the solved value back into M, finally return m
        data <- x$get()
        m <- solve(data, ...)
        x$setsolve(m)
        m    
}


## TEST SCRIPT
## b<-makeCacheMatrix(matrix(c(2,3,4,1), nrow=2, ncol=2, byrow=TRUE))
##cacheSolve(b)

