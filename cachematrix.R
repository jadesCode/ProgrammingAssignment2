## Put comments here that give an overall description of what your functions do
# This function creates a special matrix object, and it can cache its inverse
## Week 3- Assignment for R programming -July 14 2018
## Write a short comment describing this function

makeCacheMatrix <- function(x = matrix()) {
        inv <- NULL        # initialize  inv 
        set <- function(y) {  #define the set function to assign new value of matrix
          x <<- y             # in parent environment
          inv <<- NULL       # if there is a new matrix, reset inv to NULL
         }
       
        get <- function() x  # define the get fucntion - returns value of the matrix argument
            
        setinverse <- function(inverse) inv <<- inverse  # assigns value of inv in parent environment
        getinverse <- function() inv                     # gets the value of inv where called
        list(set = set, get = get, setinverse = setinverse, getinverse = getinverse)  #  enable reference to functions with $ operator
}


## Write a short comment describing this function
#This function computes the inverse of the special "matrix" returned by makeCacheMatrix.
## If the inverse has already been calculated (and the matrix has not changed),
## then cacheSolve will retrieve the inverse from the cache

cacheSolve <- function(x, ...) {
       
        inv <- x$getinverse()
        if(!is.null(inv)) {
                    message("getting cached data")
                    return(inv)
                    }
        data <- x$get()
        inv <- solve(data, ...)
         x$setinverse(inv)
         inv
}
