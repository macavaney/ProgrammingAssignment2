## These two functions that, taken together, cache the value of the
## inverse of a matrix.
##
## makeCacheMatrix is a function that returns a list containing functions
## to a) set a new matrix to test; b) get the matrix that's been
## set, c) set the inverse of the matrix, and d) get the inverse
## of the matrix. These functions are called below in cacheSolve.
##
## (NOTE: For the purposes of this assignment, I think this desription
## is all they're looking for? But while solving it I found it helpful 
## to write out a step by step of what each function element is doing.
## See the bottom for more detailed notes.)


makeCacheMatrix <- function(x = matrix()) {
        m <- NULL
        set <- function(y){
                x <<- y
                m <<- NULL
        }
        get <- function() x
        setinverse <- function(inverse) m <<- inverse
        getinverse <- function() m
        list(set = set, get = get, setinverse = setinverse, getinverse = getinverse)
}

## cacheSolve is a function that checks whether the inverse of a matrix
## is stored in a cache. If so, it returns the cached value. If not,
## it calculates the matrix inverse and stores it in the cache. (Again, 
## see below for a step by step.)

cacheSolve <- function(x, ...) {
        m <- x$getinverse()
        if(!is.null(m)) {
                message("getting cached data")
                return(m)
        }
        data <- x$get()
        m <- solve(data, ...)
        x$setinverse(m)
        m
}

## Step by step of what's going on in makeCacheMatrix:
##
## First, a variable m is defined and set to NULL. This is a placeholder
## value for the matrix inverse, and it's NULL beause it hasn't been
## set or cached yet.
## Second, set is defined as a function of y. This is a function that will
## override any previous matrices that have previously been defined. It
## does this by setting x to y in the parent environment and re-setting m
## to NULL.
## Third, get is defined as a function that returns x
## Fourth, setinverse is defined as a) a function that takes inverse as
## an argument and returns m (within this function only); and also b) 
## globally, the value of inverse. This means that you can override the
## inverse value if you call this function alone. Or, within the correct
## working of cacheSolve below, it will set the inverse value to a 
## calculation of the matrix's inverse.
## Fifth, getinverse is a function that returns m
## Finally, this function returns a list, where defined objects are 
## "tagged" as their names and their values are what's been previously
## defined. This means that x is now a function with the arguments get, 
## set, etc... These arguments are called below in cacheSolve.
##
##
## Step by step of what's going on in cacheSolve:
##
## First, m is set to x$getinverse(). This calls the function getinverse
## which was previously defined in makeCacheMatrix.
## Second, it checks to see if the value m isn't NULL. This would only 
## happen if m (the inverse) has previously been cached. If this is the
## case, it lets you know it's getting the inverse value from the cache
## rather than calculating it, and it returns that cached value.
## Third, data is set to the argument of x called get, which is a function
## that returns the value of x
## Fourth, m is set to the inverse of data (which is returns x)
## Fifth, it runs the argument of x called setinverse, using m as an 
## argument. This takes m (the inverse of x) and makes it be overriden
## by the value cached in inverse if there is a value there. If no value
## has been cached, it calculates the inverse and stores that value in m.
## Finally, it returns the value of m.

