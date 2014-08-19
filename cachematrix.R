## Created by : DGA
## first version : 15/08/2014, Version 4
## Note: This function were created using as base the functions makeVector and
## cachemean
## Matrix inversion is usually a costly computation and their may be some benefit 
## to caching the inverse of a matrix rather than compute it repeatedly. For this
## purpose two functions are created makeCacheMatrix and cacheSolve.


## makeCacheMatrix: This function creates a special "matrix" object that can  
## cache its inverse.

makeCacheMatrix <- function(x = matrix()) { # Matrix input as -> x
        m_inv<-NULL                         # m_inv is the matrix inverse and is set to NULL
        set<-function(y){                   # every time makeCacheMatrix is call
                x<<-y
                m_inv<<-NULL
        }
        get<-function() x
        setmatrix<-function(solve) m_inv<<- solve # function solve computes the inverse
        getmatrix<-function() m_inv               # of a spuare matrix.
             list(set=set, get=get,
             setmatrix=setmatrix,                 # setmatrix is use by cachesolve during its first call
             getmatrix=getmatrix)                 # getmatrix is use by cachesolve for the next calls
}


##cacheSolve: This function computes the inverse of the special "matrix" returned 
##by makeCacheMatrix above. If the inverse has already been calculated (and the 
##matrix has not changed), then cacheSolve should retrieve the inverse from 
##the cache.

cacheSolve <- function(x=matrix(), ...) { # input is the matrix created by makeCacheMatrix
        m_inv<-x$getmatrix()
        if(!is.null(m_inv)){                   # this conditional is to check if 
                message("getting cached data") # the matrix has been cache
                return(m_inv)                  
        }
        matrix<-x$get()
        m_inv<-solve(matrix, ...)
        x$setmatrix(m_inv)                    # this line store the inverse matrix in the cache as x
        m_inv
}
