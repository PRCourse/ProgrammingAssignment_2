# ProgrammingAssignment_2
cachematrix.R

# makeCacheMatrix: This function creates a special "matrix" object that can cache its inverse.
#The special "matrix" is  a list containing a function to:

# 1.set the value of the matrix
# 2.get the value of the matrix
# 3.set the value of the mean
# 4.get the value of the mean

makeCacheMatrix <- function(x = matrix()) {
    m <- NULL
    set <- function(y) {
        x <<- y
        m <<- NULL
    }
    get <- function() x
    setinverse <- function(solve) m <<- solve
    getinverse <- function() m
    list(set = set, get = get, setinverse = setinverse, getinverse = getinverse)
}

#cacheSolve: This function computes the inverse of the special "matrix" 
#returned by makeCacheMatrix above. If the inverse has already been calculated
#(and the matrix has not changed),then the cachesolve should retrieve the 
#inverse from the cache.

cacheSolve <- function(x, ...) {
    m <- x$getinverse()
    if(!is.null(m)) {
        message("getting cached data")
        return(m)
    }
    input <- x$get()
    m <- solve(input, ...)
    x$setinverse(m)
    m
}


#Test the function

test <- diag(5,5)
class(test)
det(test)
solve(test)


Cache <- makeCacheMatrix(test)
cacheSolve(Cache)
