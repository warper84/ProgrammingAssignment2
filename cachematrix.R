## Function makeCacheMatrix "wraps" its argument, a matrix, into a list of
## "interface" functions (like methods of a class). This list is intended to be
## consumed by the function cacheSolve, where these "interface" functions are
## referenced and enable neat manipulation with this "wrapped" matrix in order
## to compute the inverse to it (or return the cached value from the previous
## calculation provided the original matrix has not changed).

## Function makeCacheMatrix expects a matrix as an argument and returns a list
## of accessor functions (methods) to this matrix.

makeCacheMatrix <- function(x = matrix()) {
	inverseMatrix <- NULL
	set <- function(newX) {
		x <<- newX
		inverseMatrix <<- NULL
	}
	get <- function() x
	setInverse <- function(inverse) inverseMatrix <<- inverse
	getInverse <- function() inverseMatrix
	list(set = set, get = get, setInverse = setInverse, getInverse = getInverse)
}


## Function cacheSolve expects a list of accessor functions (methods),
## to a matrix, i.e. an output from the function makeCacheMatrix, and
## it returns an invere to the matrix.

cacheSolve <- function(x, ...) {
	inv <- x$getInverse()
	if(!is.null(inv)) {
		message("getting cached data")
		return(inv)
	}
	data <- x$get()
	inv <- solve(data, ...)
	x$setInverse(inv)
	inv
}
