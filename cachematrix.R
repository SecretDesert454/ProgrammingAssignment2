## Matrix inversion is usually a costly computation and there may be some benefit
#to caching the inverse of a matrix rather than compute it repeatedly 
#These functionscache the inverse of a matrix.

## the <<- operator which can be used to assign a value to an object in an 
#environment that is different from the current environment.

#Examples with mean of a vector:
makeVector <- function(x = numeric()) {
    m <- NULL
    set <- function(y) {
        x <<- y
        m <<- NULL
    }
    get <- function() x
    setmean <- function(mean) m <<- mean
    getmean <- function() m
    list(set = set, get = get,
         setmean = setmean,
         getmean = getmean)
    
    
}cachemean <- function(x, ...) {
    m <- x$getmean()
    if(!is.null(m)) {
        message("getting cached data")
        return(m)
    }
    data <- x$get()
    m <- mean(data, ...)
    x$setmean(m)
    m
}


vec_container <- makeVector()
vec_container$set(c(1, 2, 3, 4, 5))
cachemean_result <- cachemean(vec_container)
cachemean_result

#Now, with the inverse of a matrix

## This function creates a special "matrix" object that can cache its inverse.

makeCacheMatrix <- function(x = matrix()) {

    s <- NULL
    set <- function(y) {
        x <<- y
        s <<- NULL
    }
    get <- function() x
    setinverse <- function(solve) s <<- solve
    getinverse <- function() s
    list(set = set, get = get,
         setinverse = setinverse,
         getinverse = getinverse)
    
    
    
}


##  This function computes the inverse of the special "matrix" returned by 
#makeCacheMatrix above. If the inverse has already been calculated 
#(and the matrix has not changed), then the cachesolve should retrieve the 
#inverse from the cache.

cacheSolve <- function(x, ...) {
    s <- x$getinverse()
    if(!is.null(s)) {
        message("getting cached data")
        return(s)
    }
    data <- x$get()
    m <- solve(data, ...)
    x$setinverse(s)
    m
}

x<-rbind(c(1, -1/4), c(-1/4, 1))  
x

makeCacheMatrix(x)

matrix_container <- makeCacheMatrix()
matrix_container$set(x)
cachematrix_result <- cacheSolve(matrix_container)
cachematrix_result