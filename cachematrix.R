## Here we have two functions that cache the inverse of a matrix.
## When the inverse of a matrix is calculated for the first time,
## we store the inverse in the cache. When the inverse is asked 
## for again, and the matrix has not changed, we read directly
## from the cache instead of calculate it again, thus save computing.
## ASSUMPTION: all input matrix are invertible.

## makeCacheMatrix: This function creates a special "matrix" object
## that can cache its inverse.

makeCacheMatrix <- function(x = matrix()) {
	inv <- NULL
	set <- function(y) {
		x <<- y
		inv <<- NULL
	}
	get <- function() x
	setinverse <- function(inverse) inv <<- inverse
	getinverse <- function() inv
	list(set = set, get = get,
		setinverse =  setinverse,
		getinverse =  getinverse)
}


## cacheSolve: This function computes the inverse of the special 
## "matrix" returned by makeCacheMatrix above. If the inverse has 
## already been calculated (and the matrix has not changed), 
## then the cachesolve should retrieve the inverse from the cache.

## NOTE: Special cases of input are also considered in cacheSolve:

## a) if both matrix and cache are NULL, print "please input
## valid data" and return NULL.
## Example:
## > m <- NULL
## > cacheList <- makeCacheMatrix(m)
## > cacheSolve(cacheList,m)

## b) if makeCacheMatrix is not called before cacheSolve, 
## print "no cache exist. cache created", then calculate the 
## inverse and create the cache.
## Example:
## > m <- matrix(1:4, nrow=2, ncol=2)
## > cacheList <- NULL
## > cacheSolve(cacheList,m)

## c) if makeCacheMatrix is not called, but matrix changes,
## print "matrix has changed. cache flushed", then calculate
## the inverse and update the cache.
## Example:
## > m <- matrix(1:4, nrow=2, ncol=2)
## > cacheList <- makeCacheMatrix(m)
## > cacheSolve(cacheList,m)
## > m <- matrix(4:7, nrow=2, ncol=2)
## > cacheSolve(cacheList,m)


cacheSolve <- function(x, aMatrix = NULL,...) {
        ## Return a matrix that is the inverse of 'x'.

	  ## Check if both input matrix and cache are NULL, 
	  ## if so, print message and return NULL.
	if (is.null(x$get()) && is.null(aMatrix)) {
		message("please input valid data")
		inv <- NULL
		return(inv)
	}

	  ## Check if cache is NULL, but has a valid matrix input
	  ## if so, print message and create cache using x$set().
	else if (is.null(x$get()) && !is.null(aMatrix)) {
		message("no cache exist. cache created")
		x$set(aMatrix)
	}

	  ## Check if input matrix changes, but cache stores old 
	  ## information. If so, print message and update cache
	  ## using x$set().
	else if (!is.null(aMatrix) && (all.equal(x$get(),aMatrix) != TRUE)) {
		message("matrix has changed. cache flushed")
		x$set(aMatrix)
	}

	  ## Get cached data, if inverse is already stored, read 
	  ## from it. If it is not stored, calculate inverse using
	  ##  solve() and store it.
	inv <- x$getinverse()
	if(!is.null(inv)) {
		message("getting cached data")
		return(inv)
	}
	data <- x$get()
	inv <- solve(data)
	x$setinverse(inv)
	inv
}
	