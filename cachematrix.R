## makeCacheMatrix() create a list which include functions: 
## set & get a matrix, set & get the inverse of the matrix

makeCacheMatrix <- function(x = matrix()) {
    inv <- NULL         # init set inv value null
    set <- function(y){
        x <<- y
        inv <<- NULL    # after matrix value change, clear inv
    }
    get <- function() x
    setinv <- function(i) inv <<- i
    getinv <- function() inv
    list(set = set, get = get, setinv = setinv, getinv = getinv)
}


## cacheSolve() calculate the inverse of "matrix" that makeCacheMatrix defined
## if the inverse exist, it skip calc and return cached inverse value

cacheSolve <- function(x, ...) {
    inv <- x$getinv()     # get the inv value
    if(!is.null(inv)) {   # if inverse already cached, return it
        message("getting cached inverse")
        return(inv)
    }
    inv <- solve(x$get(), ...)  # otherwise calculate the inverse
    x$setinv(inv)               # and cache it
    inv
}
