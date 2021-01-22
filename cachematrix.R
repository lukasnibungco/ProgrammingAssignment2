## Matrix inversion is usually a costly computation. Therefore, inversion of a matrix is usually cached 
## rather than compute it repeatedly. 
## Below are a pair of functions that are used to create a special object that stores 
## a matrix and cache's its inversion


## Function: makeCacheMatrix
## Description: This function creates a special "matrix" that peforms the following operation:
## 1. set the value of the matrix
## 2. get the value of the matrix
## 3. set the value of the inversion of the matrix
## 4. get the value of the inversion of the matrix

makeCacheMatrix <- function(x = matrix()) {
        ## invMatrix : inverse matrix 
        invMatrix <- NULL # default value
        
        ##setMatrix function: set matrix and set default value for invMatrix 
        setMatrix <- function(matrix) {
                x <<- matrix ## set matrix value
                invMatrix <<- NULL ## set default inversse matrix
        }
        
        ## getMatrix function: get matrix
        getMatrix <- function() x ## return matrix
        
        ## setInvMatrix function: set inverse matrix
        setInvMatrix <- function(inverse) invMatrix <<- inverse
        
        ## getInvMatrix function: get inverse matrix
        getInvMatrix <- function() invMatrix
        
        ## list of operations for the makeCacheMatrix
        list(setMatrix = setMatrix, getMatrix = getMatrix,
             setInvMatrix = setInvMatrix,
             getInvMatrix = getInvMatrix)

}


## Function: cacheSolve
## Description: this function calculates the inverse of the special "matrix" created with the "makeCacheMatrix" function. 
## However, it first checks to see if the inversion has already been calculated. 
## If so, it gets the inversion from the cache and skips the computation. 
## Otherwise, it calculates the mean of the data and sets the value of the mean in the cache via the setmean function.

cacheSolve <- function(x, ...) {
        ## Return the inversion of the matrix 'x'
        invMatrix <- x$getInvMatrix()
        
        ## if matrix inversion exists, return cached data
        if(!is.null(invMatrix)) {
                message("getting cached data")
                return(invMatrix)
        }
        
        ##invMatrix is not yet calculated, now calculate the inverse
        
        ## First, retrieve the matrix
        matrix <- x$getMatrix()
        ## solve matrix inversion
        invMatrix <- solve(matrix, ...)
        ## save solution to inverse matrix to cache for future retrieval
        x$setInvMatrix(invMatrix) 
        ## return the solution
        invMatrix
        
}
