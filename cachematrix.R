# This program stores a matrix value and calculates its inverse if it has not yet been calculated
# it retrives the inverse from a cache if it already exists

# function name: makeCacheMatrix 

# input: a matrix
# returns: a list containing functions
#                       ----setmatrix  - sets matrix value
#                                   -----sets inverse value if a new matrix
#                       ----getmatrix  - retrive set matix value
#                       ----getinverse  - gets inverse of set matrix
#
# does:  stores matrix value, calculates inverse

makeCacheMatrix <- function(x = matrix()) {
  
    recalcinverse <- TRUE   # do we need to recalculate inverse?
    prevmatrix <- NULL     # holds the value of a previously supplied matrix
                         # if its NA it has not yet been supplied
    inversevalue <- NULL
    
    setmatrix <- function(y){  # set the matrix value
        x <<- y
        
        if (length(prevmatrix) == 0) {    # if this is the first time a matrix has been supplied
            
            prevmatrix <<- y        # set prevmatrix to the new matrix
            inversevalue <<- solve(y)  # calculate inverse of the matri
        }
        else if (!identical(y,prevmatrix)) {   # is the new matrix same as previous
            
            inversevalue <<- solve(y)           # if not calculate inverse
            prevmatrix <<- y
        }
        # otherwise no need to recalculate inverse - just use the cache

        return
    }
    
    getmatrix <- function() {  # get the matrix value
        prevmatrix   # this is the most recent matrix value supplied
    }
    
    getinverse <- function()  # get the inverse value
    {
        inversevalue
    }

    setmatrix(x)
    
    list(setmatrix, getmatrix, getinverse)  

}



# This function requires the following input
#
# x - a matrix
# followed by an arbitrary number of instances of makeCacheMatrix
#
# This function returns a list of inverses of x according to each instance of makeCacheMatrix

cacheSolve <- function(x, ...) 
  {
  
  
  funlist <- list(...)  # list of MakeCacheMatrix instances
  
  inverselist <- list()  # list of inverse matrixes to be returned
 
  
  for (i in 1:(length(funlist))) {
    
      
      instance <- funlist[i]   # individual instance of MakeCacheMatrix
      
      instance[[1]][[1]](x)  # set that instance with x 
                            # it will recalculate inverse value only if needed
      
      inverselist <- list(inverselist, instance[[1]][[3]]()) # add inverse matrix to list
      
  }  

  inverselist
  
}