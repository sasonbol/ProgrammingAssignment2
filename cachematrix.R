## These functions are to cache the process of matrix inversion which is a time-
## consuming process

## MakeCacheMatrix() function is used to cache the inversed matrices, reducing the 
## time used in calculation

makeCacheMatrix <- function(x = matrix()) {
  
  i <- NULL          #i indicates the inverse matrix which is set to NULL at first
 
  set <- function(y) {      # this function is used to superassign the original 
                            # matrix as y and the inverse matrix as null at first
    x <<- y                 
    i <<- NULL
  }
  
  get <- function() {x}        # this function is used to get the original matrix 
  
  setinverse <- function(inverse){ #this function is used to store the inverse matrix
        i <<- inverse              
  }
  
  getinverse <- function() {i} #this function is used to get the inverse matrix
  
  list(set = set,  get = get,  setinverse = setinverse,  getinverse = getinverse)
   ## a list with functions within the makeCacheMatrix 
  ## function, so they can be accessed externally
  
}



## CacheSolve() function is used to get the previously saved (cached) inverse of a 
## matrix with a message indicating that the output is a cached data
## and if the input is a new matrix it will calculate and get the inverse matrix
## and cache it for faster retrieval of previously claculated data

cacheSolve <- function(x, ...) {
                         ## Return a matrix that is the inverse of 'x'
 i <- x$getinverse()    # access the input matrix and gets the inverse matrix
  
  if(!is.null(i)) {                   #if the inverse matrix was already cached 
    message("getting cached data")    # print out this meassage
    return(i)                         # and retrieve the cached inverse matrix
  }                                   # and stop execution of the function
 
  the.matrix <- x$get()   # this part will be excuted if x$getinverse() was NULL 
                          # the get function will be used to fetch the input matrix
  
 i <- solve(the.matrix) # solve() function is used for true matrix multiplication
                        # when 1 matrix is provided as an argument, the 2nd one is
                        #taken as the identity matrix, thus the the function will
                        #return the inverse matrix.
 
 x$setinverse(i)        # stores the inverse matrix in the MakeCacheMatrix() 
 
  i                     # returns the calculated inverse matrix
}
