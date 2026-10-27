makeCacheMatrix <- function(x = matrix()){
  inv <- NULL # Holds the cached inverse; initialized to NULL
  
  #1. set: Changes the stored matrix clears the old cached inverse
  set <- function(y){
    x<<- y #Assigns new matrix to 'x' in the parent environment
    inv<<- NULL #Resets the cache because the matrix change
  }
  #2. get: Returns the stored matrix
  get <- function()x
  
  #3. setInverse: Stores the computed inverse into 'inv' in the parent environment
  setInverse <- function(inverse) inv <<- inverse
  
  #4. getInverse: Retrieve the stored inverse
  getInverse <- function() inv
  
  #Returns a list containing the 4 functions so we can access them via $
  list(set = set,
       get = get,
       setInverse = setInverse,
       getInverse = getInverse)
}
cacheSolve <- function(x,...){
  # Step A: Check if the inverse has already been calcuated
  inv <- x$getInverse()
  
  # Step B: If it exists in cache, return it immediately without recomputing
  if(!is.null(inv)){
    message("getting cached data")
    return(inv)
  }
  # Step C: If it does not exist, compute it
  data <- x$get()           #Check if the invevrse has already been calculated
  inv <-solve(data,...)     #Compute the invverse using R's built-in solve()
  x$setInverse(inv)         #Save it into the cache for next time
  inv                       #Return the inverse
}