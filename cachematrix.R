##These two functions are used to cache the inverted matrix. 
##Since in some cases, the operation of the matrix inversion can 
##be quite resource-intensive

##function makeCacheMatrix creates a list that contains the function:
##get and set the value of the matrix 
##get (getinverse) and set (setinverse) the value of inverse of the matrix

makeCacheMatrix <- function(x = matrix()) {
    inv_matrix <- NULL
    set <- function(y){
        x <<- y
        inv_matrix <<- NULL
    }
    get <- function() x
    setinverse <- function(inverse) inv_matrix <<- inverse
    getinverse <- function() inv_matrix
    list(set = set, get = get, setinverse = setinverse, 
         getinverse = getinverse)

}


##function cacheSolve returns the inverse matrix.
##Algorithm steps:
##check has been inverted matrix
##if so, return the stored inverted matrix
##otherwise, its calculation, cached and returning 
##the calculated inverse matrix

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
    inv_matrix <- x$getinverse()
    if (!is.null(inv_matrix)) {
        message("getting cached inverse matrix...")
        return (inv_matrix)
    }
    data <- x$get()
    inv_matrix <- solve(data, ...)
    x$setinverse(inv_matrix)
    inv_matrix
}

## Check
# x = rbind(c(1, 0.5), c(0.5, 1))
# i = makeCacheMatrix(x)
# i$get()
#[,1] [,2]
#[1,]  1.0  0.5
#[2,]  0.5  1.0
# cacheSolve(i)
#[,1]       [,2]
#[1,]  1.3333333 -0.6666667
#[2,] -0.6666667  1.3333333
# cacheSolve(i)
#getting cached inverse matrix...
#[,1]       [,2]
#[1,]  1.3333333 -0.6666667
#[2,] -0.6666667  1.3333333
