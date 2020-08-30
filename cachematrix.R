## Put comments here that give an overall description of what your
## functions do
#This function is designed to create a special Matrix object that can also cashe its inverse 
makeCacheMatrix <- function(x = matrix()) { 
invt <- NULL 
set <- function(y) 
{
x <<- y 
invt <<- NULL 
} 
get <- function() x 
setInverse <- function(inverse) inv <<- inverse 
getInverse <- function() invt 
list(set = set, get = get, setInverse = setInverse, getInverse = getInverse) 
}
# This function create or computes the inverse of the special matrix returned by  makeCacheMatrix.
cacheSolve <- function(x, …) { 
invt <- x$getInverse()
if(!is.null(invt)){
  #Message function  is to display information
message("gettingcacheddata")
return(invt)
}
mdata<−x$get() 
  #solve function below is used to compute the inverse of square matrix where x is a square invertible matrix
  # while solve(x) returns the inverse of x
invt <- solve(mdata,...) 
x$setInverse(invt) 
invt
}
