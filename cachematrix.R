# R Programming Week 2 Assignment: Lexical Scoping

# Validation: 
# m <- matrix(rnorm(4),nrow=2,ncol=2)
# x <- makeCacheMatrix(m)
# cacheSolve(x)
# cacheSolve(x)

# Overall these functions create matrices and subsequently move the created matrix to and from 
# the cache memory and perform operations on the data

m <- matrix(rnorm(4),nrow=2,ncol=2) # creates the 2D square matrix to be inverted in this test
print(m) # prints the created 2D square matrix to the console

# In principle, makeCacheMatrix creates the list of functions that move the data matrix and the 
# inverted data matrix to and from the cache memory

makeCacheMatrix <- function(x = numeric()) {
        invm <- NULL                                   ## initialises a NULL variable for the inverted matrix "invm" in the local memory
        set <- function(y) {                           ## creates the set function to set the data matrix and invm to the cache memory
                x <<- y                                ## sets the passed data matrix to cache memory
                invm <<- NULL                          ## initialises NULL in cache memory
        }
        get <- function() x                            ## function to get variable x from cache memory 
        setinv <- function(solve) invm <<- solve       ## function to set the value of invm in cache memory
        getinv <- function() invm                      ## get the value of invm from cache memory
        list(set = set, get = get,                     ## returns list of functions
             setinv = setinv,
             getinv = getinv)
}

# In pricniple, cacheSolve receives the data matrix (created above in x <- makeCacheMatrix(m)) and checks the cache to see if the 
# inverse of the matrix (invm) has already been calculated. If so, the value of invm stored in cache is displayed in the console. 
# If a NULL value is stored in the cache, the data matrix x (stored in cache) will be retrieved and the inverse matrix is calculated.
# This will then be stored in cache memory and finally be displayed in the console. 

cacheSolve <- function(x, ...) {
        invm <- x$getinv()                            ## retrives the value of invm into local memory from cache memory
        if(!is.null(invm)) {                          ## NOT logical operator to determine whether inverse has already been calculated
                message("getting cached data")        ## if NOT null, will display the message "getting cached data" ...
                return(invm)                          ## ... and print to console
        }
        data <- x$get()                               ## if a NULL value of invm is obtained, the data matrix is loaded to local memory
        invm <- solve(data, ...)                      ## and the inverse calculated using the solve() function
        x$setinv(invm)                                ## setinv saves the invm to cache memory
        invm                                          ## ... and print to console
}

# Example:

# m <- matrix(rnorm(4),nrow=2,ncol=2)
# > x <- makeCacheMatrix(m)
# > cacheSolve(x)
# [,1]      [,2]
# [1,]  1.0593015 1.6706482
# [2,] -0.1026943 0.9169256
# > cacheSolve(x)
# getting cached data
# [,1]      [,2]
# [1,]  1.0593015 1.6706482
# [2,] -0.1026943 0.9169256
