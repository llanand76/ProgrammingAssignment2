## makeCacheMatrix() and cacheSolve() functions together can either find inverse
## of an input square matrix object and caches result or if input matrix  
## has already inversed once in environment, gets the result from
## cache instead of recomputing the inverse matrix.

## To run : first makeCacheMatrix() on a matrix and then cacheSolve the 
## special matrix returned by makecacheMatrix()

## makeCacheMatrix() takes a matrix as input, computes inverse of matrix,
## creates list object with four functions to manipulate matrix & its inverse
## get() - gets the input matrix, 
## set(y) - assigns inverse matrix to input object
## getinverse() - gets the inversed matrix from environment
## setinverse() - uses solve to get inverse of a square matrix

makeCacheMatrix <- function(x = matrix()) {
    ## Assumption - input matrix object is square matrix    
    im <- NULL
    
    set <- function(y){
        x <<- y
        im <<- NULL
    }
    
    get <- function() x
    
    setinverse <- function(solve) im <<- solve
    
    getinverse <- function() im
    
    list(set = set, 
         get = get, 
         setinverse = setinverse,
         getinverse = getinverse)
    
}


## cacheSolve computes the inverse of special matrix returned by above 
## makeCachematrix function, if matrix is cacheSolve first time, inverse
## is computed, if matrix is called more than once, then inverse is 
## obtained from cache.

cacheSolve <- function(x, ...) {
    ## Return a matrix that is the inverse of matric 'x'
    
    im <- x$getinverse()
    
    if (!is.null(im)){
        message("getting matrix from cached data")
        return(im) 
    }
    
    data <- x$get()
    
    im <- solve(data,...)
    
    x$setinverse(im)
    
    im
}

