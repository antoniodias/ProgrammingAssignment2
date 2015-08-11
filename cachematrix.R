## These two functions allow for the creation of
##a special matrix object that can compute and store
##a cache version of its inverse matrix 


#This function allows the creation of a special kind of matrix
makeCacheMatrix <- function(x = matrix()){
  
  #Empty variable to store the inverse matrix
  s <- NULL
  
  #Function that changes the original matrix of the object 
  set <- function(y) {
    x <<- y
    s <<- NULL
  }
  
  #Function that returns the matrix
  get <- function(){
    x
  }
  
  #Function that sets the inverse matrix (but does not compute it)
  setinverse <- function(inverse){
    s <<- inverse
  }
  
  #Function that returns the inverse matrix (if already computed)
  getinverse <- function(){
    s
  }
  
  #Store the 4 functions
  list(set = set, get = get,
       setinverse = setinverse,
       getinverse = getinverse)
}

#This functions computes or returns the inverse matrix
cacheSolve <- function(x, ...) {
  #first we find if there is a stored version of the inverse matrix already in the object
  s <- x$getinverse()
  #if it exists we get that and print a message saying it was already computed
  if(!is.null(s)) {
    message("getting cached data")
    return(s)
  }
  
  #if not we have to compute it and we set it
  data <- x$get()
  s <- solve(data, ...)
  x$setinverse(s)
  s
}

##if you want you can test the functions by following the code
## We create a mtraix
a <- matrix(c(1,1,1,3,4,3,3,3,4),nrow=3,ncol=3)


#we create the special object with the function
c <- makeCacheMatrix(a)
c$get()
#there is no inverse yet
c$getinverse()

#we compute the inverse matrix
cacheSolve(c)
c$getinverse()

#and now it is stored so when we repeat it the warining appears
cacheSolve(c)

