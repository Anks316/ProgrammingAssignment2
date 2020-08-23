## Two functions that will help to cache/temporarily store the inverse of a matrix

##Creation of a special matrix which can be cached

makeCacheMatrix <- function(x = matrix()) {
a<-NULL
set<-function(y){
  x<<-y
  a<<-NULL
}
get<-function()x
setInverse<-function(inverse)a<<-inverse
getInverse<-function()a
list(set=set,get=get,
     setInverse=setInverse,
     getInverse=getInverse
}


## Inversing of the cached matrix

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
  a<-x$getInverse()
  if(!is.null(a)) {
    message("retreiving cache")
    return(a)
  }
  matr<-x$get()
  a<-solve(matr,...)
  x$setInverse(a)
  a
}
}
