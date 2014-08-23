## The functions here facilitate a caching environment generally
## for the function solve(...), and more specifically for
## calculation of the inverse of a given matrix.  Given a matrix,
## A, the inverse of A will only be directly calculated when 
## the matrix A is redefined.  

## Upon sourcing this file, the functions may be called as
## follows to define, initialize and invert a matrix A:
##
##    A <- makeCacheMatrix(matrix(...))
##    invA <- cacheSolve(A, ...)
##
## or alternaively,
##
##    A <- makeCacheMatrix()
##    A$setmat(matrix(...))
##    invA <- cacheSolve(A, ...)

## EXTRA COMMENT:
## Okay.  The scoping rules and the function of the '<<-' 
## operator are a little bit tricky, so in the function,
## I will provide comments that endeavor to explain by 
## example, how these things work.  

## The function makeCacheMatrix(...) initializes 
## 'caching' conditions for a matrix, ensuring that the 
## matrix inverse is only directly calculated per call to 
## this function.  Otherwise, the matrix inverse already stored 
## in memory is used.  To do this, it returns a list of 
## functions to initialize and retrieve the matrix and 
## its inverse in an 'object oriented' like manner.  

makeCacheMatrix <- function(x = matrix()) {
    ## The line 'invx <- NULL' is important because it 
    ## ensures that the 'makeCacheMatrix' is the first
    ## environment where invx is defined.  If we didn't 
    ## have this line here, then, for example, the 
    ## statement 'invx <<- inversex' in the function 
    ## setinv() below would keep looking ever upward in the 
    ## enclosing environments for a definition of invx.  
    ## Being (most likely) unable to find one, it would
    ## then grab a (hopefully existing) definition from the 
    ## global environment (i.e. the R console).  Instead, 
    ## the invx variable initialized below in this function, 
    ## and not any other, will be the one that is modified by 
    ## any 'invx <<- ...' in the functions whose definitions 
    ## are nested in this makeCacheMatrix() below.  A similar
    ## argument holds for x as it is defined as an input 
    ## argument to this makeCacheMatrix(x) function.  
    ##   
    invx <- NULL
    
    setmat <- function(y) {
        x <<- y
        invx <<- NULL
    }
    
    getmat <- function() x
    
    setinv <- function (inversex) {
        invx <<- inversex
    } 

    getinv <- function () invx
    
    list(setmat=setmat, getmat=getmat, setinv=setinv,
         getinv=getinv)
     
    
}


## Write a short comment describing this function

cacheSolve <- function(x, ...) {
    ## Return a matrix that is the inverse of 'x'
    
    ## By lexical scoping rules, the line below will retrieve
    ## the matrix invx in the function makeCacheMatrix() and 
    ## assign it to xinv.  If cacheSolve() is called 
    ## immediately after a call to makeCacheMatrix() or 
    ## setmat(), invx is set to NULL by the first statement 
    ## in makeCacheMatrix() or the second statement inside 
    ## the setmat() definition, respectively.  Otherwise, 
    ## invx (and xinv) will not be NULL because they will have
    ## been defined by a previous call to this cacheSolve() as 
    ## per the lines below.  
    xinv <- x$getinv()

    ## If xinv is not NULL then use/return cached inverse.
    if(!is.null(xinv)) {
        message("Using cached inverse.")
        return(xinv)
    }

    ## If xinv is NULL, calculate the inverse.  
    mat1 <- x$getmat()
    xinv <- solve(mat1, ...)

    ## Now use lexical scoping rules to store the 
    ## calculated xinv from the two lines of code above into 
    ## invx in the function cacheSolve(), thus redefining invx 
    ## so it is no longer NULL.  
    x$setinv(xinv)
    xinv
}
