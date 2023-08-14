## Put comments here that give an overall description of what your
## functions do

## Write a short comment describing this function
# the first function stores the cache for the matrix inversion.
makeCacheMatrix <- function(x = matrix()) {
        
        inv <- NULL
        set <- function(y) {
                x <<- y
                inv <<- NULL
        }
        get <- function() x
        setinverse <- function(inverse) inv <<- inverse
        getinverse <- function() inv
        list(set = set, get = get,
             setinverse = setinverse,
             getinverse = getinverse)

}


## Write a short comment describing this function

# the function 2 uses cache from the function 1 which stores for the matrix 
# need to be inverted and tries provide the matrix inversion results
cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
        inv <- x$getinverse()
        if(!is.null(inv)) {
                message("getting cached data")
                return(inv)
        }
        matrix_to_invert <- x$get()
        inv <- solve(matrix_to_invert, ...)
        x$setinverse(inv)
        inv
}

my_Matrix <- makeCacheMatrix(matrix(c(34, 50, 7, 89), 2, 2))
my_Matrix$get()
cacheSolve(my_Matrix)
