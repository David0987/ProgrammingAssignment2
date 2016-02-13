#The structure of my function is similar to the examples given by the instructors. mean() is replaced by 'solve' and vector is replaced by matrix()
#The rest of the functions are the same.

## Write a short comment describing this function
#inverse_m stands for inverse of a matrix; replaces 'm'
#solve(x) is used to replace mean(x)

makeCacheMatrix <- function(x = matrix()) {
        inverse_m <- NULL
        set <- function(y) {
                x <<- y
                inverse_m <<- NULL
        }
        get <- function() x
        seti <- function(inverse_input) inverse_m <<- inverse_input
        geti <- function() inverse_m
        list(set = set, get = get,
             seti = seti,
             geti = geti)
        
}


## Write a short comment describing this function

cacheSolve <- function(x) {
        ## Return a matrix that is the inverse of 'x'
        inverse_m <- x$geti()
        if(!is.null(inverse_m)) {
                message("getting cached data")
                return(inverse_m)
        }
        data <- x$get()
        inverse_m <- solve(data)
        x$setmean(inverse_m)
        inverse_m
}
