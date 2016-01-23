
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
        ## Use Invers instead of mean as described in call notes
        setinverse <- function(inverse) m <<- inverse  
        getinverse <- function() m
        list(set = set, get = get,
             setinverse = setinverse,
             getinverse = getinverse)
}


## Calculate the inverse using the solve function as described in assignment notes

cacheSolve <- function(x, ...) {
       
        m <- x$getinverse()
        if(!is.null(m)) {
                message("getting cached data")
                return(m)
        }

        data <- x$get()
## Use solve function as recommended instead of mean in sample code
        m <- solve(data, ...)
        x$setinverse(m)
        m
}
 
