## Below functions caches the inverse of a Matrix
## It may take too long to compute a function especially if done in a loop
## Its more effficient to cache the inverse using scoping rules of R
## The inverse can then be stored and looked up when needed rather than recomputed each time 

## creates a special "matrix" object that can cache its inverse

makeCacheMatrix <- function(x = matrix()) {
	m <- null
	set <- function(y) {
	x <<- y
	m <<- null
	}
	get <- function() x
	setInvert <- function(inverse) m <<- inverse
	getInvert <- function() m
	list(set = set, get = get,
		setInverse = setInverse,
		getInverse = getInverse)
}


## computes inverse of special "matrix" returned by makeCacheMatrix above
## If the inverse has already been calculated and matrix isn't changed
## then retrieve inverse from the cache

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
	  m <- x$getInvert()
        if(!is.null(m)) {
                message("getting cached data")
                return(m)
        }
        data <- x$get()
        m <- solve(data, ...)
        x$setInvert(m)
        m
}
