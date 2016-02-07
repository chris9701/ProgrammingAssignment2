## Caching the Inverse of the Matrix
## I have created two functions below that are used to create a special
## object that stores a matrix and then caches the inverse

makeCacheMatrix <- function(x = matrix()) {
	inv <- NULL
	set <- function(y) {
		x <<- y
		inv <<- NULL
	}
	get <- function () x
	setInverse <- function(inverse) inv <<- inverse
	getInverse <- function () inv 
	list(set = set,
		 get = get,
		 setInverse = setInverse,
		 getInverse = getInverse)
}


## This function computes the inverse of the special matrix created above

cacheSolve <- function(x, ...) {
        inv <- x$getInverse()
        if (!is.null(inv)) {
        	message("getting cached data")
        	return(inv)
        }
        mat <- x$get()
        inv <- solve(mat, ....)
        x$setInverse(inv)
        inv
}
