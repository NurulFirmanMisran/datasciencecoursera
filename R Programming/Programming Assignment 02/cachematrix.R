## Student Name: Nurul Firman Bin Misran
## Email: nurulfirman.misran@gmail.com

## makeCacheMatrix, this function creates a special "matrix" object that can cache its inverse
## 1. set the value of the matrix
## 2. get the value of the matrix
## 3. set the inverse of the matrix
## 4. get of inverse of the matrix

makeCacheMatrix <- function(x = matrix()) {
        ## initialise inverse matrix
        invMat <- NULL
        
        ## 1. set the value of the matrix
        set <- function(y){
                x <<- y
                invMat <<- NULL
        }
        
        ## 2. get the value of the matrix
        get <- function() x
        
        ## 3. set the value of the inverse of the matrix
        setInv <- function(inverse) invMat <<- inverse
        
        ## 4. get the value of the inverse of the matrix, getInverse function
        getInv <- function() invMat
        
        list(set=set, get=get, setInv=setInv, getInv=getInv)
        
}

## cacheSolve, this function computes the inverse of the special "matrix" returned by makeCacheMatrix function.
## If the inverse has already been calculated (and the matrix has not changed), then the cacheSolve should retrieve the inverse from the cache


cacheSolve <- function(x, ...) {
        
        #get matrix inverse
        invMat <- x$getInv()
        
        #if the inverse are cached, retrieve from the cache and return the inverse matrix
        if(!is.null(invMat)){
                message("getting cached data.")
                
                #Return inverse matrix
                return(invMat)
        }
        
        #if inverse is not cache, calculate, set and return the inverse.
        data <- x$get()
        invMat <-solve(data)
        x$setInv(invMat)
        
        ##Return inverse matrix
        invMat
        
}