##    The first function, makeCacheMatrix, returns a list of 4 functions:
## set, get, setinv, and getinv functions 
## takes as input and matrix ,x, and returns a list of 4 functions:
## set, get, setinv, and getinv functions .
##  The <<- operator is used to set the value of i to null in the parent (global)
##  Environment


makeCacheMatrix <- function(x = matrix()) {
        i <- NULL
        set <- function(y) {
                x <<- y
                i <<- NULL
        }
        get <- function() x
        setinv <- function(inv) i <<- inv
        getinv <- function() i
        list(set = set, get = get,
             setinv = setinv,
             getinv = getinv)

}


## This function checks to see if we have already cached the inverse of the
## matrix x.  If it is the cache , it alerts the user that it is getting the
##  cached inverse before displaying it.  If not, it computes the inverse using the
## Solve function .  
##  If it is called again, the cacheMatrix function has it now stored in cache
## and it wont have to be re-calcualted

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
        i <- x$getinv()
        if(!is.null(i)) {
                message("getting cached data")
                return(i)
        }
        data <- x$get()
        i <- solve(data )
        x$setinv(i)
        i

}
