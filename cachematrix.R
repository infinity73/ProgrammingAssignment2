
## these functions: create a special object which contains a matrix and
## evaluate and cache its inverse. If inverse of the matrix already
## exists, it is not re-evaluated.



## the function below creates a list which contains four functions;
## 'set' function sets the value of matrix and 'get' function gets the
## vaue of that matrix.'setinverse' function sets the value of inverse 
## and 'getinverse' function gets the vaue of the inverse. 

makeCacheMatrix <- function(x = matrix()) {
        i <- NULL
        set <- function(y) {
                x <<- y
                i <<- NULL
        }
        get <- function() x
        setinverse <- function(inverse) i <<- inverse
        getinverse <- function() i
        list(set = set, get = get,
             setinverse = setinverse,
             getinverse = getinverse)

}


## the function below evaluates the inverse of a matrix which is pres-
## ent in the special object created when 'makeCacheMatrix' function
## is used. if inverse is already set, it is not re-evaluated.

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
        
        i <- x$getinverse()
        if(!is.null(i)) {
                message("getting cached data")
                return(i)
        }
        data <- x$get()
        i <- solve(data, ...)
        x$setinverse(i)
        i
}
