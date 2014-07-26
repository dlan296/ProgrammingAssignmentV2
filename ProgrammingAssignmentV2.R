
#########################################################################
### makeCacheMatrix holds four functions for setting / getting values ###
### stored in functions for enhanced performance                      ###
#########################################################################

makeCacheMatrix <- function(x = matrix()) {
  
  ### allocate space for inverse 
  inv <- NULL
  
  ### set / get values ###
  set <- function(y) x <<- y; inv <<-  NULL
  get <- function() x
  set_inverse <- function(inverse) inv <<- inverse
  get_inverse <- function() inv
  
  ### output ###
  list( set = set
      , get = get
      , set_inverse = set_inverse
      , get_inverse = get_inverse)
}


#############################################################################
### cacheSolve takes an argument of type makeCacheMatrix and does the     ###
### following:                                                            ###
### 1) checks to see if the makeCacheMatrix argument already holds the    ###
###    inverse matrix, if so, it will exit and return the pre computed    ###
###    value                                                              ###
### 2) if the value is not computed the function will compute and return  ###
###    the inverse matrix                                                 ### 
#############################################################################

cacheSolve <- function(x, ...) {
  
    ### extract value from chached matrix ###
    inv <- x$get_inverse()
    
    ### check to see if it has already been computed ###
    if(!is.null(inv)) {
      message("getting cached data")
      return(inv)
    }
    
    ### if not, compute inverse matrix ###
    mat <- x$get()
    inv <- solve(mat)
    x$set_inverse(inv)
    
    ### ouput inverse ###
    inv
}
