##makeCacheMatrix assigns special vector for the supplied matrix
##cacheSolve calculates inverse of supplied matrix depending on wether the matrix has been supplied before or not
## To test the assignment first call makeCachematrix on your choice of matrix and store it in a variable.
## Then call cacheSolve with this variable repeatedly


##makeCacheMatrix takes in a matrix and returns a list of functions to set,get the matrix and set/get the inverse of the matrix
## x is the input matrix
## inverse is the inverse of x
makeCacheMatrix <- function(x = matrix()) {
            inverse<- NULL
            set <- function(y) {
                x <<- y
                inverse <<- NULL
            }
            get <- function() x
            setinverse <- function(inv) inverse <<- inv
            getinverse <- function() inverse
            list(set = set, get = get,
                 setinverse = setinverse,
                 getinverse = getinverse)
}


##cacheSolve takes in a list of matrix functions on the matrix 'x' and returns the inverse of the matrix 'x'
## x is the input matrix
## inv is the inverse of x
## If inv has been previously calculated then directly its value is returned.
## In either case appropriate messages are delivered
cacheSolve <- function(x){
        ## Return a matrix that is the inverse of 'x'
    
        inv <- x$getinverse()
        if(!is.null(inv)) {
            message("getting cached data")
            return(inv)
        }
        data <- x$get()
        message("calculating data first time")
        inv <- solve(data)
        x$setinverse(inv)
        inv
    
}
