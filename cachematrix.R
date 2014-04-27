## Pair of functions that cache the inverse of a matrix

## Creates a special "matrix" object that can cache its inverse

makeCacheMatrix <- function(x = matrix()) {
        inverseMat <- NULL
        set <- function(y) {
                x <<- y
                inverseMat <<- NULL
        }
        get <- function() x
        setInverse <- function(inverse)  inverseMat <<- inverse
        getInverse <- function() inverseMat
        list(set = set, get = get,
             setInverse = setInverse,
             getInverse = getInverse)
}


## computes the inverse of the special "matrix" returned by makeCacheMatrix above. 
## If the inverse has already been calculated (and the matrix has not changed), then the cachesolve should retrieve the inverse from the cache

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
        inverseMat <- x$getInverse()
        if(!is.null(inverseMat))
        {
                print("getting cached data")
                return(inverseMat)
        }
        data <- x$get()
        inverseMat <- solve(data)
        x$setInverse(inverseMat)
        inverseMat
}
