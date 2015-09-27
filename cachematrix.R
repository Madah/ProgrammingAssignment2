## Put comments here that give an overall description of what your
## functions do

## Write a short comment describing this function
## September 2015



## "makeCacheMatrix" is a function that creates a 'special matrix' and
## a set of functions (closures) that create a set of functionallities 
## to handle the 'special matrix'
## These functionalities are:
##     set: sets the value of the new matrix
##     get: returns and shows the matrix created
##     setinv: sets (caches) the inverse of the matrix
##     getinv: returns and shows the inverse of the matrix


makeCacheMatrix <- function(x = matrix()) {
    inv <- NULL     #inverse of a matrix
    
    #set the values for the new matrix 
    set <- function(y) {
        x <<- y
        inv <<- NULL #resets the value of the inverse of the new matrix
    }
    #get the values of the matrix 
    get <- function() x 
    
    #set the value of the new inverse matrix
    setinv <- function(inverse) inv <<- inverse
    
    #get the value of the inverse matrix 
    getinv <- function() inv

    list(set = set, get = get,
         setinv = setinv,
         getinv = getinv)

}


## Write a short comment describing this function


## "cachesolve" is a function that calcutes the inverse of a matrix
## passed as a parameter ('x').
## It first checks if an inverse is already cached, if not then 
## calculates the inverse of the given matrix and caches it.


cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
    
    #checks if the inverse is already calculated
    inv <- x$getinv()
    if(!is.null(inv)) {
        message("getting cached data")
        return(inv)
    }
    
    #calculates the new inverse matrix to be cached
    data <- x$get()
    #if the matrix is a square invertible matrix
    # the inverse matrix is returned
    inv <- solve(data, ...) 
    x$setinv(inv) #'caches' the value of the inverse
    inv
}



