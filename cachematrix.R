## Put comments here that give an overall description of what your
## functions do

## create a list has 4 functions about get inverse of the matrix

makeCacheMatrix <- function(x = matrix()) {
	inv <- NULL
	set <- function(y) {
		if (!is.matrix(y)) {
			message("arg is not matrix")
			return
		}
		x <<- y
		inv <<- NULL
	}
	get <- function() {
		if (is.matrix(x)) {
			x
		} else {
			NULL
		}
	}
	setInverse <- function(inverse) {
		inv <<- inverse
	}
	getInverse <- function() {
		inv
	}
	list(set = set, get = get,
		 setInverse = setInverse,
		 getInverse = getInverse)
}


## cache inverse matrix to list x

cacheSolve <- function(x, ...) {
	## Return a matrix that is the inverse of 'x'
	inv <- x$getInverse()
	if (!is.null(inv)) {
		message("getting cached data")
		return (inv)
	}
	data <- x$get()
	inv <- solve(data, ...)
	x$setInverse(inv)
	inv
}

