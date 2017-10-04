## The below two functions help cache the inverse of the matrix and 
## returns it when required repeatedly without having to compute it again everytime.


## This function creates a special "matrix" object that can cache its inverse.

makeCacheMatrix <- function(x = matrix()) {
	invr <- null
	set <- function(y) {
		x <<- y
		invr <<- NULL
	}
	get <- function() x
	setinverse <- function(inverse) invr <<- inverse
	getinverse <- function() invr
	list(set = set, get = get,
	     setinverse = setinverse,
	     getinverse = getinverse)
}


## This function computes the inverse of the special "matrix" returned by makeCacheMatrix above. 
## If the inverse has already been calculated (and the matrix has not changed), 
## then the cachesolve should retrieve the inverse from the cache.

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
	invr <- x$getinverse()
	if(!is.null(invr)) {
		message("getting cached data")
		return(invr)
	}
	data <- x$get()
	invr <- solve(data, ...)
	x$setinverse(invr)
	invr
}
