## These two functions create a "special" matrix and stores its inverse in the cache,
## so that the same inverse does not need to be computed repeatedly

## This function creates a "special" matrix that is able to cache its inverse

makeCacheMatrix<-function(x=matrix()){
        inv<-NULL
	set<-function(y){
		x<<-y
		inv<<-NULL
		}
	get<-function(){
		x
		}
	setinv<-function(solve){
		inv<<-solve
		}
	getinv<-function(){
		inv
		}
	list(set=set,get=get,setinv=setinv,getinv=getinv)
        }

## This function computes the inverse of the "special" matrix created by the makeCacheMatrix function
## If the inverse was already calculated, this function will retrieve it from the cache instead of 
## calculating it again

cacheSolve<-function(x,...){
        inv<-x$getinv()
	if(!is.null(inv)){
		message('getting cached data')
		return(inv)
		}
	data<-x$get()
	inv<-solve(data,...)
	x$setinv(inv)
	inv
        }
