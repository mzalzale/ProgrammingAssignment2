## Assignment of week #3 of the R Programming course.
## Lexical scoping demonstration via caching the inverse of a matrix.
## Inversion is done using the solve() function so assuming that the matrix is 
## square and invertible.

## makeCacheMatrix() builds a set of functions and returns the functions within
## a list to the parent environment. It usses lexical scoping to contain a 
## complete copy of the environment.
makeCacheMatrix <- function(x = matrix()) {
    # We initialize the two objects, x in the function argument, and i below.
    i <- NULL #
    # Now we need to define the getters and setters.
    set <- function(y) {
        # Use the <<- operator to assign the input argument y to the x object
        # in the parent environment.
        x <<- y
        # Then assign the value of NULL to the i object in the parent 
        # environment. This action clears any value of i that had been cached 
        # by a prior execution of cacheSolve() so that whenever x is re-set, 
        # the value i of the cached result is cleared, forcing the code to 
        # recalculate rather than get an outdated and hence wrong value.
        i <<- NULL
    }    
    # Define the get() function, using the lexical scoping in R, since x is 
    # not defined within get(), R gets the x value from the parent environment 
    # of makeCacheMatrix().     
    get <- function() x
    # Now we have to define the setters and getters for the inverse i.
    # Define the setInverse(): since i is defined in the parent environment 
    # and we need to access it after setInverse() completes the codes uses the 
    # <<- operator to assign the input argument to the value of i in the 
    # parent environment.
    setInverse <- function(solve) i <<- solve
    # And to get the inverse taking advantage of lexical scoping in R to find 
    # the correct symbol i and retrieve its value.
    getInverse <- function() i
    # So far we have getters and setters of the data objects (x,i) in the 
    # makeCacheMatrix() object. Now we assign each of these functions as an 
    # element within a list() and return it to the parent environment. We name 
    # all the elements so that we can access the functions by name, 
    # e.g. myMatrix$get(), etc...
    list(set = set, get = get, setInverse = setInverse, getInverse = getInverse)
}


## cacheSolve() completes the above function makeCacheMatrix() because it is 
## here that the inverse of the matrix is actually calculated. cacheSolve() 
## starts with a single argument (matrix) and an ellipsis that allows the 
## caller to pass additional arguments. Then the function attempts to retrieve 
## the inverse of the matrix from the object passed in as the argument.
cacheSolve <- function(x, ...) {
    # Try to get the inverse from the objects passed in as the argument
    # First call the getInverse() 
    i <- x$getInverse() 
    # Then check if the returned i is NULL. Since makeCacheMatrix() sets the 
    # cached inverse to NULL whenever a new matrix is set into the object, 
    # if the value here is not equal to NULL, we have a valid, cached inverse 
    # matrix that we can return to the present environment. 
    if(!is.null(i)) {
        message("getting cached data")
        return(i)
    }
    # If the result is NULL, cacheSolve() gets the vector from the input object,
    # calculates the inverse using solve(), and uses setInverse() function on 
    # the input object to set the inverse in the input object, and then returns 
    # the value of the mean to the parent environment by printing the inverse 
    # matrix i.
    data <- x$get()
    i <- solve(data, ...)
    x$setInverse(i)
    i
}
