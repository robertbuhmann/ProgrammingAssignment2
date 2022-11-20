## The 'makeCacheMatrix' function creates an R object that stores a dataframe
##and its inverse. It does this by building a set of functions and storing
##these functions as a list in the parent environment.

makeCacheMatrix <- function(x = matrix()){
  m <- NULL
  
  #Initialize empty dataframe and empty vector
  
  set <- function(y) {
    x <<- y
    m <<- NULL 
    
    #Define set() function, takes the argument y and assigns it to the object
    #'x' in the parent environment; takes a NULL and assigns it to the 'm'
    # object in the parent environment (clears values for 'm' stored during a
    #prior execution of 'cacheSolve'.
    
  }
  get <- function() x
  
  #retrieve 'x' from parent environment and use it as the getter/accessor 
  #for dataframe 'x'
  
  setsolve <- function(solve) m <<- solve
  
  #define the setter/mutator for 'solve'
  
  getsolve <- function() m
  
  #define the getter/accessor for 'm'
  
  list(set = set, get = get,
       setsolve = setsolve,
       getsolve = getsolve)
  
  #Assign each function as an element within a list and return the list to the 
  #parent environment. This allows us to use the '$' operator to access the
  #functions by name when using the 'cacheSolve' function later.
  
}


## The 'cacheSolve' function retrieves the inverse of a dataframe stored
##by the object 'makeCacheMatrix'

cacheSolve <- function(x, ...) {
  m <- x$getsolve()
  
  #retrieve mean for object passed in as an argument.
  
  if(!is.null(m)) {
    message("getting cached data")
    return(m)
    
    #check to see if object stored as 'm' is 'NULL', if TRUE,  it returns the
    #cached inverse matrix
    
  }
  data <- x$get()
  m <- solve(data, ...)
  x$setsolve(m)
  m
  
  #if !is.null is FALSE, the input dataframe is retired, the inverse 
  #calculated and returned to the parent environment by printing the inverse 
  #dataframe
}