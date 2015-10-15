## This set of functions handles an object for storing a matrix and 
## and its inverse. The inverse is only calculated when it is asked
## for, and then stored in a cache to avoid more computational work
## if it should be needed again.

## Create a CacheMatrix object containing a matrix and an empty 
## placeholder for the inverse. Given an object created with
##    xCache <- makeCacheMatrix(x)
## the matrix x can be extracted with xCache$get(),
## and it can be exchanged for another one with xCache$set(x). 

makeCacheMatrix <- function(x = matrix()) {
	invx <- NULL
	set <- function(y) {
		x <<- y
		invx <<- NULL
	}
	get <- function() x
	setinv <- function(theinv) invx <<- theinv
	getinv <- function() invx
	list(set = set, get = get, 
		setinv = setinv, getinv = getinv)
}


## Use this function to extract the inverse from a CacheMatrix 
## object. If xCache is such an object, the inverse of the matrix
## inside is retrieved with invx <- cacheSolve(xCache). If the 
## inverse is not present, it is automatically calculated and
## stored in xCache, before beeing returned.

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
	invx <- x$getinv()
	if(!is.null(invx)) {
		return(invx)
	}
	xmatrix <- x$get()
	invx <- solve(xmatrix, ...)
	x$setinv(invx)
	invx
}
