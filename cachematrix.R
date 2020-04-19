## Below are two functions that create a special matrix vector,
## and then cache its inverse. 

## The first function, makeCacheMatrix, creates a special "vector,"
## which is really a list containing a function to:
##	1. set the value of the matrix
##	2. get the value of the matrix
##	3. set the inverse of the matrix
##	4. get the inverse of the matrix 

makeCacheMatrix <- function(x = matrix()) {
	i <- NULL
	set <- function(y) {
		x <<- y
		m <<- NULL
	}
	get <- function() x
	setInverse <- function(inverse) i <<- inverse
	getInverse <- function() i
	list(set = set, get = get,
		setInverse = setInverse,
		getInverse = getInverse)
}


## The second function finds the inverse of the special matrix. If
## the inverse has already been found, it gets the mean from the
## cahche and skips the calculation. 

cacheSolve <- function(x, ...) {
	i <- x$getInverse()
	if(!is.null(i)) {
		message("getting cahced inverse matrix!")
		return(i)
	}
	data <- x$get()
	i <- solve(data, ...)
	x$setInverse(i)
	i
}
