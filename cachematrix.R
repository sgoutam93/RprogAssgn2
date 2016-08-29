## makeCacheMatrix function make a matrix object to cache the inverse matrix
## input argument is an invertible matrix

makeCacheMatrix <- function(x = matrix()) {
        inv <- NULL #emptied to store inverse for future use
        set <- function(y){
                #this function will be used for setting new matrix
                x <<- y
                inv <<- NULL
                message(inv)
        }
        get<-function() x #to get the prcossed special matrix to be used together with the cachesolve function
        setinv<- function(newinv) inv <<-newinv # to set the newly calculated Inverse matrix
        getinv<-function () inv  #to get the Invers matrix
        list(set = set, get = get,
             setinv=setinv,
             getinv=getinv)
        
 # end of makeVector function       

}

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
        inv<- x$getinv()
        if(!is.null(inv)){
                message("pulling the cache data")
                return (inv)
        }
        #if Inv is empty Invers matrix has to be created
        data <- x$get()
        inv<-solve(data,...) #creat invers matrix
        x$setinv(inv) #pushed for cache
        inv #return the inverse matrix
}
