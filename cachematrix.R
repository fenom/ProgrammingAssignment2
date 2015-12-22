## Put comments here that give an overall description of what your
## functions do

## makeCacheMatrix is a more sophisticated version of a plain matrix.
## cacheSolve reterns the inverse of a matrix made with makeCacheMatrix.

## Write a short comment describing this function

## makeCacheMatrix() is a function containing four functions,
## so it's like a class in other programming languages.
## It takes a matrix as its sole argument.
## It stores the matrix and its inverse in a different environment.
## The four functions are getting and setting the matrix itself,
## and getting and setting its inverse.

makeCacheMatrix <- function(x = matrix()) {
	i <- NULL ## Initialize inverse to NULL
	set <- function(y) {
		## Function resetting the object with a new matrix and erasing the inverse
		## The two variables are assigned to a different environment
		x <<- y
		i <<- NULL
	}
	get <- function() x ## Function returning the matrix
	setinverse <- function(inverse) i <<- inverse ## Function setting the inverse
	getinverse <- function() i ## Function returning the inverse
	## Return a list of the four functions
	list(set = set, get = get, setinverse = setinverse, getinverse = getinverse)

}


## Write a short comment describing this function

## cacheSolve() is a function that returns the inverse of the matrix
## in an object created by makeCacheMatrix().
## The function takes a makeCacheMatrix() object as its argument,
## along with other arguments to be passed on to solve()

cacheSolve <- function(x, ...) {
	i <- x$getinverse() ## First get the inverse that's already in the object
	if(!is.null(i)) { ## If the inverse already exists, simply return it
		message("getting cached data")
		return(i)
	}
	data <- x$get() ## Get the raw matrix from the object
	i <- solve(data, ...) ## Calculate the inverse of the matrix
	x$setinverse(i) ## Set to inverse in the object so that it can be used later
	i
        ## Return a matrix that is the inverse of 'x'
}
