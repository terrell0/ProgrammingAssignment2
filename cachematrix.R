
## The first function, makeCacheMatrix creates a special "matrix", which 
## is really a list containing a function to

##    set the values of the matrix
##    get the values of the matrix
##    set the value of the inverse
##    get the value of the inverse



makeCacheMatrix <- function(x = matrix()) {
    m <- NULL
        set <- function(y) {
                x <<- y
                m <<- NULL
        }
        get <- function() x
## Changed sample code to to use inverse instead of mean
        setinv <- function(inverse) m <<- inverse  
        getinv <- function() m
        list(set = set, get = get,
             setinv = setinv,
             getinv = getinv)
}


## Calculate the inverse using the solve function as described in assignment notes

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'

        m <- x$getinv()
        if(!is.null(m)) {
                message("getting cached data")
                return(m)
        }
## Use matrix
        mat.data <- x$get()
## Use solve function as recommended instead of mean in sample code
        m <- solve(mat.data, ...)
        x$setinv(m)
        return(m)
}
