## MakeCache Matrix:
# Creates a special empty matrix that shall be used as cache
# Creates a list of functions: 
# 1 set - sets a value for x outside of the function 
# enviroment , 
# 2 get - gets the value of x, 
# 3 setsolve - calculates the inverse matrix of using the solve() function, 
#getsolve - gets the inverse matrix

makeCacheMatrix <- function(x = matrix()) {
        m <- NULL
        set <- function(y){
                x <<- y
                m <<- NULL
        }
        get <- function() x
        setsolve <- function(solve) m <<- solve
        getsolve <- function () m
        list(set = set, get = get,setsolve = setsolve, 
              getsolve = getsolve)
}


## This function checks to see if we already have the inverse matrix calculated
## in the cache. To do that we use the getsolve() to see if we have a non-NULL 
## "m". If we do, it returns the value of m, else it calculates the inverse
## matrix using the function setsolve() and returns it

## BEFORE USING IT REMEMBER YOU NEED TO 
## i) initialise makeCachematrix using 
## ii) b <- makeCacheMatrix() and then use b$set() to set a  square matrix 
## that will be inverted

cacheSolve <- function(x, ...) {
        m <- x$getsolve()
        if(!is.null(m)){
                message("getting cached data")
                return(m)
        }
        data <- x$get() 
        m <- solve(data,...)
        x$setsolve(m)
        m
        
}
