## MakeCache Matrix creates a lists of functions that: set the value of a 
## matrix,get the value of a matrix, set the value of the inverse, get the value
## of the inverse

makeCacheMatrix <- function(x = matrix()) {
        m <- NULL
        set <- function(y){
                x <<- y
                m <<- NULL
        }
        get <- function() x
        setinverse <- function(solve) m <<- solve
        getinverse <- function () m
        list( set = get, get = get,setinverse = setinverse, 
              getinverse = getinverse)
}


## This function checks to see if we already have the inverse matrix calculated
## in the cache. If yes it returns it's value, else it calculates it

cacheSolve <- function(x, ...) {
        m <- solve(x)
        if(!s.null(m)){
                message("getting cached data")
                return(m)
        }
        m <- solve(x) 
        m
}
