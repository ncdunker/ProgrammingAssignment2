## The first function, makeCacheMatix creates a list containing a function to 
## produce and set the inverse of a matrix.
##
## The function establishes and clears the variables to hold the matrix and its inverse
## These variables are in essence promoted to a parent environment to be made available for 
## use in the next function. 
makeCacheMatrix <- function(matrix_input = matrix()) {
        matrix_inverse <- NULL
        set <- function(y) {
                matrix_input <<- y
                matrix_inverse <<- NULL
        }
## This part of the makeCacheMatrix function gets the matrix input and computes the inverse.
        get <- function() matrix_input
        setsolve <- function(solve) matrix_inverse <<- solve
        getsolve <- function() matrix_inverse
        list(set = set, get = get,
             setsolve = setsolve,
             getsolve = getsolve)
}
## The cacheSolve function checks to see if the inverse has already been calculated. 
## If so, it gets the inverse from the cache. Otherwise, it calculates the inverse 
## of the matrix and sets the value of the inverse in the cache via the setsolve function. 
cacheSolve <- function(matrix_input, ...) {
        matrix_inverse <- matrix_input$getsolve()
        if(!is.null(matrix_inverse)) {
                message("getting cached data")
                return(matrix_inverse)
        }
        data <- matrix_input$get()
        matrix_inverse <- solve(data, ...)
        matrix_input$setsolve(matrix_inverse)
        matrix_inverse
}
