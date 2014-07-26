##  This  file  containts the solution for week 2 assignment for R programming course from Coursera.
## It consists of two functions which are dependent on each other. First function named makeCacheMatrix 
## takes a matrix as input and caches it . Second function(i.e. cachesolve) takes matrix and variable number of input. 
##It uses first function to cache the inverse output in outside of function scope.


## makeCacheMatrix function takes a matrix as input. It has 4 internal functions. set function sets passed value to 
## to original matrix and copy the variable in a different than current environment. This will esnure 
## that varibale is available outside the method scope.setinverse copies the passed inverse to the variable and makes it
##accessible outside function scope. get returns original matrix and getinverse returns inversed matrix.

makeCacheMatrix <- function(x = matrix()) {
	m <- NULL
	set <- function(y) {
		x <<- y
		m <<- NULL
	}
	get <- function() x
	setinverse <- function(inverse) m <<- inverse
	getinverse <- function() m
	list(set = set, get = get, setinverse = setinverse, getinverse = getinverse)

}


## This method is the main method which is invoked to get the inverse of a 
##matrix. After the check , it tries to get the inverse of passed matrix. If the
##response for getInverse is null then it means that this is the first
##invocation and so inverse for this matrix is not available in cache. In that
##case it create an Inverse and store it using setInverse.

cacheSolve <- function(x, ...) {
## Return a matrix that is the inverse of 'x'	
	m <- x$getinverse()
	if (!is.null(m)) {
		message("getting cached data")
		return(m)
	}
##if inverse is null
	data <- x$get()
##Going to inverse the pased matrix.
	m <- solve(data, ...)
	x$setinverse(m)
	m
}
