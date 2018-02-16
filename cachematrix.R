#WE prepare a list of function calls from which we can set or get the elements of a
#square matrix. All unique 'vectors' created by this function have the option to cache
# the inverse in a separate environment (make sure the inverse IS THE INVERSE!!)

makeCacheMatrix <- function(xmat){
     
     #should we set the matrix in the main function, its inverse 
     #is initialized to 'NULL'. Otherwise placed in separate environment
     #when setMat is called.
     matSpace <- NULL
     setMat <- function(ymat){
          xmat <<- ymat
          matSpace <<- NULL
          
     }
     
     #gets the value of the matrix or inverse from the list
     getMat <- function() xmat
     getInvmat <- function() matSpace
     
     #option to manually set the value,
     #or used to pass the calcualtion, and puts it in the cache
     setInvmat <- function(invxmat) matSpace <<- invxmat
     
     #this is what is returned: a list of functions within the 'vector' 
     #calling the respective values
     
     list(setMat = setMat, getMat = getMat,
          setInvmat = setInvmat, getInvmat = getInvmat)
}    
#########################################################################################
#########################################################################################

#This function accepts the list generated from makeCacheMatrix and calulates the inverse
#from xmat$getMat() or reads an existing value of xmat$getinvMat() from cache.

cacheSolve <- function(xmat, ...) {
     matSpace <- xmat$getInvmat()
     # If the inverse has been cached....
     if(!is.null(matSpace)) {
          message("getting cached data")
          return(matSpace)
     }

     # If the inverse has NOT been cached...
     data <- xmat$getMat()
     matSpace <- solve(data, ...)
     xmat$setInvmat(matSpace)
     matSpace
}