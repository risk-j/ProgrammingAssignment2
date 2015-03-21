
## makecacheMatrix creates a special "matrix" object that can cache its inverse.
makeCacheMatrix <- function(x = matrix()) {
    i <- NULL #initialize the inverse matrix
    
    ## set the value of x and discard the inverse matrix
    set <- function(y = matrix()) {
        x <<- y
        i <<- NULL
    }
    
    get <- function() x
    
    ## set the value of inverse matrix (i)
    setinverse <- function(inverse) i<<-inverse
    
    getinverse <- function() i
    
    list(set = set, get = get, setinverse = setinverse, getinverse = getinverse)
     
}



## This function computes the inverse of the special "matrix" 
## returned by makeCacheMatrix above. 
## If the inverse has already been calculated (and the matrix has not changed), 
## then the cachesolve should retrieve the inverse from the cache
cacheSolve <- function(x=matrix(), ...) {
    ## Return a matrix that is the inverse of 'x'
    i <- x$getinverse()
    
    if(!is.null(i)) {
        message("getting cached data")
        return(i)
    }
    
    ## get the matrix to be solved
    data<-x$get()
    
    ## sovle the matrix
    i <- solve(data)
    
    ## set the value of inverse
    x$setinverse(i)
    
    i
    
}
