## Put comments here that give an overall description of what your
## functions do

## Write a short comment describing this function


## Creates a special matrix object that can cache its inverse
makeCacheMatrix <- function(x = matrix()) {
        #set m to a null value
        inverted_matrix<-NULL
        
        # Method to set the matrix
        set<-function(y){
                x<<-y
                inverted_matrix<<-NULL
        }
        
        # method to get the matrix and return it
        get<-function() x
        
        #method to set the inverse
        setmatrix<-function(solve) inverted_matrix<<- solve
        
        #method to get the matrix and return
        getmatrix<-function() inverted_matrix
        
        #return a lits of the methods
        list(set=set, get=get,
             setmatrix=setmatrix,
             getmatrix=getmatrix)

}


## Write a short comment describing this function
## Compute the inverse of the special matrix returned by "makeCacheMatrix". 
## If the inverse has already been cached, then the "cachesolve" should 
## retrieve the inverse from the cache.

cacheSolve <- function(x, ...) {
        
        #return the inverse of x
        inverted_matrix<-x$getmatrix()
        
        #if the matrix inverse is cached, return the cached matrix
        if(!is.null(inverted_matrix)){
                
                message("Using Cached Data Rather than Recomputing")
                
                #Retrun the cached Matrix
                return(inverted_matrix)
        }
        
        # the following will only be run if the matrix is not cached
        
        # get the matrix and calculate its inverse
        matrix<-x$get()
        inverted_matrix<-solve(matrix, ...)
        
        #set the inverse to the object
        x$setmatrix(inverted_matrix)
        
        
        ## Return a matrix that is the inverse of 'x'
        message("The matrix was not cached, and was inverted and cached for the next use")
        inverted_matrix
        
}
