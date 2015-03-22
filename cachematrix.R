## The function cachematrix() is intended for:
## - From a given vector, create a square invertible matrix. This
##   matrix is stored in the cachematrix() environment (R_GlobalEnv)
## - From a given matrix, verify if this is invertible and save it
##   also in the cachematrix() environment (R_GlobalEnv)
## - For objects as lists or non-numeric vectors or matrices,  
##   cachematrix() stops after display a message explaining why the
##   the object was not accepted

makeCacheMatrix <- function(x = matrix()) {

# Function checks the class of the given object 'x', and displays object
  x_class <- class(x) 
  print("You entered object:")  
  print(x)

# Function verifies if object has the appropriate class to be processed
# If not: program ends
  if(x_class !="integer" & x_class != "numeric"
     & x_class != "matrix"){
    return(print("Your object is not a vector or a matrix"))
  } 
# Function verifies if object has numeric element only
# If not: program ends
  if(!is.numeric(x)){
    return(print("Your objec has non numeric elements"))
  }  
# For a matrix object...
  if(x_class == "matrix"){
  # Function verifies if matrix is square. If not: program ends
    if(nrow(x) != ncol(x)){
      return(print("Your matrix is not square"))
    }
  # Function verifies if matrix is invertible. If not: program ends    
    if(det(x) == 0 ){
      return(print("Your matrix has no inverse"))
    }  
  # Function computes inverse matrix. Result is stored in R_GlobalEnv
  # under the name CMAT. A list of the working directory content is 
  # displayed to show the cache matrix is there, and program ends. 
    CMAT <<- x
    print("You entered a numeric matrix. Cache-matrix 'CMAT' was")
    print("created in the Make_Matrix() function's environment:")
    print(parent.env(environment()))
    return(ls(parent.env(environment())))
     
  }
# Function creates a square matrix from an entered vector
# If the vectors's length is unable to produce a square matrix
# function matrix() will recycle the missing/extra numbers to
# produce the square matrix, displaying a warning message 
  mat_len <- round(sqrt(length(x)))
  square_matrix <- matrix(x, nrow = mat_len, ncol = mat_len)
# The matrix produced is diplayed and stored in R_GlobalEnv
# as 'CMAT'. A list of the working directory content is 
# displayed to show the cache matrix is there, and program ends.
  print("Your entered numeric vector produced the next matrix:")
  print(square_matrix)
  CMAT <<- square_matrix
  print("Cache-matrix 'CMAT' with above matrix was created and")
  print("stored in the Make_Matrix() function's environment:")
  print(parent.env(environment()))
  return(ls(parent.env(environment()))) 
  
}



## The function cachesolve() is intended for:
## - Find the inverse for the matrix 'CMAT' generated with cachematrix()
##   and store inverse in cachesolve() environment (R_GlobalEnv) under
##   object 'CMAT_inv'
## - If the inverse for 'matrix 'CMAT' is required again, cachesolve()
##   retrieves the previously computed inverse stored under 'CMAT_inv'
##   and returns this as result
## - This function will also compute the inverse of any non-singular
##   square matrix (non-zero determinant)  
## _ This function requires a numerical square matrix as input. 

cacheSolve <- function(x, ...) {

# Function displays the matrix entered
  print("For matrix:")
  print(x)

# This function helps to obtain the inverse of a matrix
  x_inv <- function(y){
    print("the inverse matrix is:")
    solve(y)
  }

# Calculates inverse of entered matrix, when no matrix has been cached
# displays result, and program ends
  if(!exists("CMAT")){
    return(print(x_inv(x)))
  }

# Calculates inverse of entered matrix when this is not the cached matrix
# displays result, and program ends
  if(!identical(x,CMAT)){
    return(print(x_inv(x)))  
  }
  
# When the entered matrix is the cached matrix:
  if (!exists("CMAT_inv")) {
  # If inverse matrix has not been calculated    
    CMAT_inv <<- solve(CMAT)
    print("the inverse matrix CMAT_inv is computed:")
    print(CMAT_inv)
    print("and stored in the cacheSolve() function's environment:")
  } 
  else {
  # If the inverse matrix has been previously calculated, (i.e object
  # 'CMAT_inv' exists in working directory), a message is displayed,
  # stored inverse matrix 'CMAT_inv' is returned, and program ends.
    print("the inverse matrix CMAT_inv:")
    print(CMAT_inv)
    print("already exists in cacheSolve() function's environment:")
  }  
#  A list of the working directory content is displayed to show 
# inverse of the cache matrix is also in cacheSolve() environment
  print(parent.env(environment()))  
  ls(parent.env(environment()))  

}
