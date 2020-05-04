#caching the inverse of a matrix
#function of creates a matrix object

makeCacheMatrix <- function(x = matrix()) {
        n <- NULL
        
        #setting the matrix
        set <- function(y) {
                x <<- y
                n <<- NULL
        }
        
        #getting the matrix
        get <- function() {
                x
        }
        
        #setting the inverse of the matrix
        setInverse <- function(inverse) {
                n <<- inverse
        }
        
        #getting the inverse of the matrix
        getInverse <- function() {
                n
        }
        
        #return a list of the methods
        list(set = set,
             get = get,
             setInverse = setInverse,
             getInverse = getInverse)
}




## The following function calculates the mean of the special "vector" created with the above function.

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
        n <- x$getInverse()
        
        #returning the inverse if its already set
        if (!is.null(n)) {
                message("getting cached data")
                return(n)
        }
        
        #matrix from our object
        m <- x$get()
        n <- solve(m, ...)
        #set the inverse to the object
        x$setInverse(n)
        n #return
}

