## This function accepts a matrix and converts it into an object that has a set of functions
## that can manipulate its data

makeCacheMatrix <- function(x = matrix()) {
## Make a new vector called inverse
inv <- NULL
## This function sets a new value for the matrix
set <- function(y) {
    x <<- y
    inv <<- NULL
}

## This function gets the current matrix
get <- function() x

## This function sets a new value for the inverse
setinv <- function(inverse) inv <<- inverse
    
## Gets the current value of the inverse
getinv <- function() inv

list(set=set, get=get, setinv=setinv, getinv=getinv)
}


## This function checks if a inverse value exists and returns that instead if calculating a
## a new inverse

cacheSolve <- function(x, ...) {
	inv <- x$getinv()
	if(!is.null(inv)) {
		message('getting cached data')
		return(inv)
	}
	matrix <- x$get()
	inv <- solve(matrix, ...)
	x$setinv(inv)
	return(inv)
}
