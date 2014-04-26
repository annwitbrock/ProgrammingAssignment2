## The pair of functions makeCacheMatrix() and cacheSolve()
## allow an invertible matrix and its inverse to be cached.
## The inversion function used is solve() 

## makeCacheMatrix - returns a cacheable invertible matrix.
##        x: is an invertible matrix
##        set() stores the matrix
##        get() retrieves the matrix
##        setinverse caches the result of of the inversion function
##        getinverse retrieves any cached result

makeCacheMatrix <- function(x = matrix()) {
        inv <- NULL
        
        # store the matrix
        set <- function(mx){
                x <<- mx
                inv <<- NULL
        }
        get <- function() x
        
        # cache the inverse matrix
        setinverse <- function(s) inv <<- s
        getinverse <- function() inv
        
        list(set = set,
             get = get,
             setinverse = setinverse,
             getinverse = getinverse)
}


## cacheSolve - returns the cached inverse of a matrix.
##        x:  is a cacheable invertible matrix defined by calling
##            makeCacheMatrix(M), where M is an invertible matrix
##
         
cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
        # get cached inverse if it exists
        inv <- x$getinverse()            
        if(!is.null(inv)){
                message("getting cached data")
                return(inv)
        }
        
        # create a cached inverse
        data <- x$get()          # obtain the matrix
        inv <- solve(data, ...)  # invert the matrix
        x$setinverse(inv)        # cache the inverse
        inv
}
