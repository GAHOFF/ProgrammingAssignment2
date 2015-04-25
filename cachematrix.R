## By caching the inverse of a matrix, the following functions eliminate the 
## need to repeatedly calculate the inverse of the matrix.

## makeCacheMatrix is a function that contains 4 subfunctions that:
## 1)set - changes the matrix stored in the main function
## 2)get - returns the matrix stored in the main function
## 3)setmatrix - stores the matrix as variable m
## 4)getmatrix - returns the matrix stored in m

makeCacheMatrix <- function(x = matrix()) {
        m <- NULL
        set <- function(y) {
                x <<- y
                m <<- NULL
        }
        get <- function() x
        setmatrix <- function(solve) m <<- solve
        getmatrix <- function() m
        list(set = set, get = get,
             setmatrix = setmatrix,
             getmatrix = getmatrix)
}

## cacheSolve is a function that computes the inverse of the matrix defined in
## makeCacheMatrix after checking to see if the inverse had been previously
## calculated and stored in m.  If the value was previously calculated the
## message "getting cached data" is received with the inverse matrix previously
## calculated.

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
        m <- x$getmatrix()
        if(!is.null(m)){
                message ("getting cached data")
                return(m)
        }
        data <-x$get()
        m <- solve(data, ...)
        x$setmatrix(m)
        m
}
