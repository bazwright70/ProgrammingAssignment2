# --makeCacheMatrix-- allows you to pass it a matrix to create a special matrix object which
#     can cache the matrix and the calculation of a matrix inversion.
#
# --cacheSolve-- will take the special matrix object and produce a matrix inversion and cache it.
#     If will check the current cache of the special matrix object to  see if the values have changed
#     before running the inversion function and return cached values values are unchanged.


##This function creates a special "matrix" object that can cache its inverse.

makeCacheMatrix <- function(x =matrix())  {
     inv <<- NULL                    # set value of matrix inverse to NULL
     
     get <- function() x             # function to return object matrix
     set <- function(y) {            
         m <<- y                     # function to cache the current matrix
         inv <<- NULL }              # and set The inversion to NULL
     
  getInv <- function(x) inv          # function to check matrix inversion cache
  setInv <- function(x) inv <<- x    # function to cache the inversion of the current matrix
  
  list(set=set, get=get,setInv=setInv,getInv=getInv)

}

#    ## Return a matrix that is the inverse of 'x'

cacheSolve <- function(x,...) {
  
  cache <- x$getInv()   # assign any cached matrix inversion to a variable
  
  if(!is.null(cache) && m==x$get()){     # Test to see if a cached matrix is stored and 
     print("Getting Cached data")        # the matrix has not changed
     return(cache)
  }

  
  dat <- x$get()           # get the current matrix and assign to a variable
  x$set(dat)               # cache the current matrix and set invers cache to NULL
  inverse <- solve(dat)    # run current matrix through solve function ad assign variable
  x$setInv(inverse)        # cache matrix inverse and display
  inverse
}
