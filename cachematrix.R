### The following two functions are used to invert a matrix and store the inverse in a cache. ###

### makeCacheMatrix creates of list of functions that may be used to store the inverse of a matrix. ###
makeCacheMatrix <- function(x = matrix()) {
 inv <- NULL                                    ### Initialize inv to NULL in local environment.         ###
 get <- function() x                            ### Returns the value of x into local environment.       ###
 setinv <- function(inv_mat) inv <<- inv_mat    ### Sets inv equal to inv_mat in cache.                  ###
 getinv <- function() inv                       ### Returns the value of inv into local environment.     ###
 list(get=get, setinv=setinv, getinv=getinv)    ### Return list of the functions that have been defined. ###
}


### cacheSolve inverts a matrix by either calculating the inverse if the inverse does not already exist, or reading it ###
### from the cache of the list created by makeCacheMatrix if it was already created. ###
cacheSolve <- function(x, ...) {
 inv <- x$getinv()                              ### Set inv equal to the value of inv in the cache matrix (NULL if not already calculated) into the local environment. ###
 if(!is.null(inv))                              ### If inv has already been calculated, then get the value of inv from the cache matrix.                               ###
 {
    message("getting cached data")
    return(inv)                                 ### If getting inv from the cache, then return that value and exit the function. ###
 }
                                                
### If inv was not in the cache matrix, then do the following. ###
 mat_to_invert <- x$get()                       ### Get the original matrix that is to be inverted from the cache matrix and put into the local environment. ###
 inv <- solve(mat_to_invert, ...)               ### Set inv equal to the inverse of the matrix to be inverted and put into the local environment.            ###
 x$setinv(inv)                                  ### Now set inv in the cache matrix.                                                                         ###
 inv                                            ### Return inv and exit function.                                                                            ###
}
