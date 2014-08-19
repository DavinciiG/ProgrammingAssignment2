## Matrix inversion is usually a costly computation and their may be some benefit 
## to caching the inverse of a matrix rather than compute it repeatedly. 


## makeCacheMatrix: This function creates a special "matrix" object that can  
## cache its inverse.

makeCacheMatrix <- function(x = matrix()) {
        m<-NULL
        set<-function(y){
                x<<-y
                m<<-NULL
        }
        get<-function() x
        setmatrix<-function(solve) m<<- solve
        getmatrix<-function() m
        list(set=set, get=get,
             setmatrix=setmatrix,
             getmatrix=getmatrix)
}


##cacheSolve: This function computes the inverse of the special "matrix" returned 
##by makeCacheMatrix above. If the inverse has already been calculated (and the 
##matrix has not changed), then cacheSolve should retrieve the inverse from 
##the cache.

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
}
