
## Creates a special matrix objec that can cache its inverse 
makeCacheMatrix <- function(a = matrix()) {
	I <- NULL			## initializing I as an object within the makeCacheMatrix() environment 
	set <- function(b) {	
		a <<- b		## assign the input argument to the a object in the parent environment
		I <<- NULL		## assign the NULL to the I object in the parent environment
	}
	get <- function() a	## get a
	setinverse = function(inverse) I <<- inverse	## define the setter for the inverse I
	getinverse = function() I	## define the getter for the inverse I
	list(set = set, get=get,	## create a new object by returning a list to the parent environment
	     setinverse = setinverse, 
	     getinverse = getinverse)
}

## computes the inverse of the special matrix returned by makeCacheMatrix
cacheSolve <- function(a, ...) {
	I <- a$getinverse()	## if the inverse has already been calculated, then get cached data
	if(!is.null(I)) {
		message("getting cached data")
		return(I)		
	}
	data <- a$get()         ## otherwise compute the inverse
	I <- solve(data, ...)
	a$setinverse(I)
	return(I)			
}
