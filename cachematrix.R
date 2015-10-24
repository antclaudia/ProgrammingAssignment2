## The function makeCacheMatrix creates a special "matrix" that remembers
## its inverse when it has been previously computed.
## makeCacheMatrix returns a list with function members:
## set(Y) that sets the matrix to use and resets the inverse to NULL.
## get() that gets the current value of the matrix to use.
## setinverse(Z) that sets the value of the inverse of the matrix to use.
## getinverse() that gets the value of the inverse of the matrix to use. 

makeCacheMatrix<-function(X=matrix()){
        inversematrix<-NULL
        set<-function(Y){
                X<<-Y
                inversematrix<<-NULL
        }
        get<-function(){
                return(X)
        }
        setinverse<-function(Z){
                inversematrix<<-Z
        }
        getinverse<-function(){
                return(inversematrix)
        }
        list(set=set, get=get,
             setinverse=setinverse,
             getinverse=getinverse)
}

## The function cacheSolve computes the inverse of the 'matrix' created by
## the function makeCacheMatrix. This function checks first if 
## the inverse has already been calculated. If that's the case, it gets the inverse
## from the cache, and skips the computation. Otherwise, it computes the inverse of 
## the given matrix using the function solve, and sets the value of the inverse in 
## the cache via the setinverse function.


cacheSolve <- function(X, ...) {
        inversematrix<-X$getinverse()
        if(!is.null(inversematrix)){
                message("getting cached data")
                return(inversematrix)
        }
        Amatrix<-X$get()
        inversematrix<-solve(Amatrix,...)
        X$setinverse(inversematrix)
        return(inversematrix)
        ## Return a matrix that is the inverse of 'X$get()'
}
