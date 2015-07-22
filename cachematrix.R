
makeCacheMatrix <- function(x = matrix()) {
    # x is a regular invertible matrix
    # the function will in four steps do the following:
    # 1.set a matrix,2. get a matrix,3. set the inverse,4. get the inverse
    m<- NULL
        
    set <- function(y) {
            x <<- y
            m <<-NULL
        }
        # set function assigns the value to the object in a different environment
        
    get <- function() x
        # get function stores the value of matrix x
    
    setmatrix <- function(solve) m<<-solve
        #setmatrix function assigns the value of the inverse matrix to m
    
    getmatrix <- function() m
        # getmatrix function stores the value of inverse
    
    list (set=set, get=get, setmatrix=setmatrix, getmatrix=getmatrix)
        # overall function returns the list of four functions
}


cacheSolve <- function(x, ...) {
    #x here is the output from the MakeCacheMatrix function, e.g. the list of functions

    m <- x$getmatrix()
    # retrieve the value of m from the MakeCacheMatrix output, e.g. the inverse of the matrix x
    
    if(!is.null(m)) {
        message("Getting cached data")
        return(m)
    }
    #test whether the matrix already exists - is different from NULL object - 
    #if yes, return the inverse without other computation
    
    data <- x$get()
    m <- solve(data, ...)
    x$setmatrix(m)
    m
    # else create a new variable data which stores the matrix in cache,
    # then calculate its inverse and store the inverse in cache.
    # return the value of the inverse matrix
}
