## Put comments here that give an overall description of what your
## functions do

## Write a short comment describing this function

#This function assigns values to a matrix (set) 
#Then defines some functions to handle this matrix. 
#The handling functions defined are - 
#To fetch the matrix (get)
#To check whether the values of matrix have changed or not (getflag) 
#To fetch the values of matrix
#To store (to cache) the inverse in an object defined in another environemnt using <<- operator (setinv) 
#To fetch the cached inverse matrix (getinv)
#Finally, outputs of the above are made into a special matrix - a list

makeCacheMatrix <- function(x = matrix()) {

 mxinv  <- NULL 
 change_flag <- 1

 set <- function(y){ 
  if(all.equal(x,y)){
   change_flag <<- 0
  }
  else{
   x <<- y  # assigns the formal argument y to x 
   mxinv <<- NULL # initialize the inverse 
  }
 } 

 #get the input matrix
 get <- function(){
  x
 }
 
 #get the change flag
 getflag <- function(){
  change_flag
 }


 #get the inverse 
 getinv <- function(){
  mxinv
 }

 #assigns the inverse to a object "mxinv" which is from another environment
 setinv <- function(inv){ 
  mxinv <<- inv
 }
 
#The list which is the special matrix object with all necessary functions to create and handle a matrix 
 list(setmatrix = setmatrix,getmatrix = getmatrix,setinv = setinv,getinv=getinv,getflag = getflag)

}

## Write a short comment describing this function

#This function  is to calculate the inverse of the given matrix. But before calculating, it checks whether it has been calculated before - for same matrix- 
#If so, it retrieves teh same. Otherwise it will calculate from teh matrix values.

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
 
 mxinv <- x$getinv()
 change_flag <- x$getflag

 if(!is.null(mxinv) && change_flag == 0  ){
  message(" Getting cached inverse")
   return(mxinv)
 }

 #Calculation  of the inverse the very first time (or after each change of the matrix)
 mx <- x$get()
 mxinv <-solve(mx,...)  # compute the inverse
 x$setinv(mxinv) # cache it
 mxinv # return the inverse to the caller
}
