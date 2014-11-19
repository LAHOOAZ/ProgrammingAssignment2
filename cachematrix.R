## These functions allow you to caclulate
## and store the value of an inverse matrix
## if a matrix has already been inverted
## the function returns the stored value
## rather than recalculating.
##
## Usage Example:
## exMatrix <- matrix(1:4, 2:5)
## matrixObj1 <- makeCacheMatrix(exMatrix)
## cacheSolve(matrixObj1)


## makeCachMatrix create an object
## that can be used to get & set a matrix object
makeCacheMatrix <- function(myMatrix = matrix()) {
  
  inverseMatrix <- NULL
  
  set <- function(givenMatrix){
    myMatrix <<- givenMatrix
    inverseMatrix <<- NULL
  }
     
  #returns the value of the matrix passed in
  get <- function() myMatrix 
  
  #stores the value of the inverse Matrix
  setInverse <- function(givenMatrix) 
    { inverseMatrix <<- givenMatrix }
   
  #returns the value of the inverse matrix
  getInverse <- function() inverseMatrix
  
  list(set = set, get = get, setInverse = setInverse,
       getInverse = getInverse)
  
}


## Use the make cache matrix function to store
## an inverse matrix once it is calculated

cacheSolve <- function(x, ...) {
  #access the object created by makeCacheMatrix
  inverseMatrix <- x$getInverse()
  
  #if it is not null return the stored value
  if(!is.null(inverseMatrix)){
    message("getting cached data")
    return(inverseMatrix)
  }
  
  #else we are going to calculate and store
  matrix <- x$get()
  
  #calculate the inverse
  inverseMatrix <- solve(myMatrix, ...)
  
  #set/store the inverse
  x$setInverse(inverseMatrix)
  
  #return the value of the inverse matrix
  inverseMatrix 
  }
