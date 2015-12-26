### makeCacheMatrix creates a "matrix" object that can cache its inverse
#### makeCacheMatrix has 1 optional argument
####   -m : a matrix, value assigned on creation of the object
#### makeCacheMatrix has 4 functions/methods
####   -get : returns m
####   -getinv : returns the inverse of m
####   -set : set the value of m
####   -setinv : set the value of the inverse of m
###Example: 
#### m1<-makeCacheMatrix(matrix(1:4,ncol=2))

makeCacheMatrix <- function (m=matrix()){
   #on creation, inv is set to NULL
   inv<- NULL   
   #set is a function setting the value of m
   set <- function (y){
      m<<-y
      inv<<- NULL
   }
   #get returns m
   get <- function () m
   #setinv sets the inverse using caching
   setinv <- function (invm) inv<<-invm
   #getinv returns the inverse
   getinv <- function () inv
   list (set = set, get = get , setinv = setinv , getinv = getinv)
}

### cacheSolve has 1 argument
####  -m : a cacheMatrix object
####  this function checks if the inverse (m$inv) has already been computed 
####  if the inverse has already been calculated (and the matrix has not changed), 
####    then the cachesolve should retrieve the inverse from the cache.
####  else it calculates the inverse and sets the value in the cache via the setinv function 
### Example (see MakeCacheMatrix for the definition of m1): 
### cacheSolve(m1)


cacheSolve <- function (m,...){
   linv<-m$getinv()
   #checking if the inverse has already been computed
   if (!is.null (linv)){
      message("getting cached data")
      return(linv)
   }
   mm<-m$get()
   linv<-solve(mm,...)
   m$setinv(linv)
   linv
}


## Put comments here that give an overall description of what your
## functions do

## Write a short comment describing this function

#makeCacheMatrix <- function(x = matrix()) {

#}


## Write a short comment describing this function

#cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
#}