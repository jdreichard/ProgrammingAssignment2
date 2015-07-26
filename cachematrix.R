#-----------------------------------------------------------
# MAKECACHEMATRIX FUNCTION
# Make a cached version of a matrix passsed to the function
#-----------------------------------------------------------
makeCacheMatrix <- function(x = matrix()) {
  #-------------------------
  # Set the Inverse to Null
  #-------------------------
  i = NULL
  
  #------------------
  # 1. SET FUNCTION
  #------------------
  set = function(y) {
    # Set x to the value passed into the 
    x <<- y
    i <<- NULL
  }
  #------------------
  # 2. GET FUNCTION
  #------------------  
  get = function() {
    x
  }
  #-----------------------------------------
  # 3. SETI FUNCTION
  #    Set the inverse to what is passed
  #-----------------------------------------
  seti = function(iv) {
    i <<- iv 
  }
  #-----------------------------------------
  # 4. GETI FUNCTION
  #    Return the inverse
  #-----------------------------------------
  geti = function() {
    i
  }
  
  #------------------------------------------------------
  # Return the list of functions
  #------------------------------------------------------
  return(list(set=set, get=get, seti=seti, geti=geti))
}

#----------------------------------------------------------
# CACHESOLVE FUNCTION
# Compute the inverse of a matrix; use the cached inverse
# if it is available and return it.
#----------------------------------------------------------
cacheSolve <- function(x, ...) {

  # Get the inverse from the cache, if possible
  inverse = x$geti()
  
  #----------------------------------------------------------
  # Check to see if the inverse is null or return it
  #----------------------------------------------------------
  if (!is.null(inverse)){
    message("Retrieving Matrix from the Cache")
    # Return the inverse
    return(inverse)
  }
  
  #----------------------------------------------------------
  # Retrieve the matrix using the get() function
  #----------------------------------------------------------
  cmat <- x$get()
  
  #----------------------------------------------------------
  # Set the inverse of the matrix using the seti() function
  #----------------------------------------------------------
  x$seti(inverse)
  
  #----------------------------------------------------------
  # Perform error checking in case the matrix is not square
  # or the seti() function returns an error message
  # First, set the inverse variable to the inverse of the
  # cmat (cachedmatrix) object
  #----------------------------------------------------------
  tryCatch( 
    
    {
      # Solve the Inverse of the Matrix and set it to the Cache Value
      inverse <-solve(cmat,...)
    },
    
    # If there is an error, report it
    error = function(err) {
      message("Error")
      message(err)
      
      # Kill the function and return NA
      return(NA)
    },
    
    # If there is a warning, report it
    warning = function(war) {
      message("Warning")
      message(war)
      
      # kill the function and return NA
      return(NA)
    },
    
    # If there is no error, set the inverse
    finally = {
      # If there are no errors, set the matrix in the cache
      x$seti(inverse)
    }
    
  )  
  #----------------------------------------------------------
  # Complete the function and return the inverse
  #----------------------------------------------------------
  return(inverse)
}

#------------------------------------------------------------------
# Sources for these functions include Stackoverflow, github, and
# and other resources on the web, which all aided in the
# completion of this assignment.
#-----------------------------------------------------------------
x1 = rbind(c(4, 7), c(2, 6))
m = makeCacheMatrix(x1)
m$get()
cacheSolve(m)