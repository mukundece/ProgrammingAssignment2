## Code uses 2 functions to come up with a solution for Programming Assignment2.
## Basic idea is to store the inverse of the matrix and its original matrix
## Storing the original matrix helps detect if the given matrix has changed or not
## If matrix has changed, compute the new matrix inverse and returns it
## Else return the stored matrix inverse from Cache 

 
## makeCacheMatrix function creates a special "matrix" object which is a list
## containing the following functions:
## set: store the value of given matrix
## get: Return the value of given matrix
## setinv: store the value of inverse matrix and its original matrix
## getinv: return the value of inverse matrix and its original matrix 
## cac_mat_inv is cached matrix inverse and orig_mat is its original matrix

makeCacheMatrix <- function(x = matrix()) {
        cac_mat_inv <- matrix(data=NA,nrow=nrow(x),ncol=ncol(x))
        cac_x       <- matrix(data=NA,nrow=nrow(x),ncol=ncol(x))   
        get <- function() x
        set <- function(y=matrix()) {
                x <<- y
        }
        setinv <- function(mat_inv=matrix(),orig_mat=matrix()) {
                cac_mat_inv <<- mat_inv
                cac_x <<- orig_mat
        } 
        getinv <- function() {
                list(cac_mat_inv, cac_x)
        }
        list(get = get, set = set, 
             setinv = setinv, getinv = getinv)
}


## cacheSolve function fetches the stored inverse matrix and its original matrix
## If the given matrix is same as the stored original matrix, then returns the stored 
## matrix inverse. If not, code computes the inverse using solve function, stores 
## the new matrix inverse and its original matrix and finally returns the computed 
## matrix inverse
 
cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
        new_mat     <- x$get()
        result      <- x$getinv() ## fetch the cached matrices
        cac_mat_inv <- result[[1]]
        cac_x       <- result[[2]]  
        if(!(all(is.na(cac_x))) && is.matrix(cac_x) && is.matrix(new_mat) && dim(cac_x) == dim(new_mat) && all(cac_x == new_mat)) {
	          print("Retrieving matrix inverse from Cache")
                cac_mat_inv       ## return the cached inverse
        } else {
                print("Computing matrix inverse using solve function")
                mat_inv <- solve(new_mat)  ## compute inverse for given matrix
                x$setinv(mat_inv, new_mat) ## Cache the new matrices
                mat_inv                    ## return the computed inverse
        }
}
