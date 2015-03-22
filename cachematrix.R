# The overall objective of this program is to speed up the calculation of Inverse of a Matrix. To achieve that, it uses the caching of the program and makes a lookup before every calculation to find a matching result.


## The first function, makeCacheMatrix creates a "matrix", which is really a list of function to
## 1. set the value of the matrix
## 2. get the value of the matrix
## 3. set the Inverse Matrix
## 4. get the Inverse Matrix

makeCacheMatrix <- function(x = matrix()) {
	i <- NULL
        setMatrix <- function(y) {
                x <<- y
                i <<- NULL
        }
        getMatrix <- function() x
        setInverse <- function(inv) i <<- inv
        getInverse <- function() i
        list(setMatrix = setMatrix, getMatrix = getMatrix,
             setInverse = setInverse,
             getInverse = getInverse)
}


## The following function calculates the Inverse of the "matrix" created with the above function. However, it first checks to see if the result has already been calculated and available in Cache. If so, it gets the mean from the cache and skips the computation. Otherwise, it calculates the inverse of the data and sets the value in the cache via the setInverse function.

cacheSolve <- function(x, ...) {
    i <- x$getInverse()
        if(!is.null(i)) {
                message("getting cached Matrix")
                return(i)
        }
        data <- x$getMatrix()
        i <- solve(data, ...)
        x$setInverse(i)
        i
		## Return a matrix that is the inverse of 'x'
}
