makeCacheMatrix <- function(x = matrix()) {
  # function takes a matrix as an parameter  
  # it contains 4 separate functions:
  # --- 1) sets value of matrix
  # --- 2) gets value of the matrix
  # ----3) set value of the inverse (created with solve function)
  # --- 4) gets the value of the inverse
  s <- NULL  
  # here we're specifying that s will be an undefined and we've eliminated 
  # any cached value associated with "s"
 
  set <- function(y) {
    # Function 1: here we reassign the value of x (whereever it might be cached) to equal y
    # s is again null (undefined)
    x <<- y 
    s <<- NULL
  }

  get <- function() {
    x
  }
  # this 2nd function retrieves the value of x and returns it
  
  setSolve <- function(inverse) {
    s <<- inverse 
  }
  #this function sets the cached vaue of s to equal the inverse we have calculated through Solve
  
  getSolve <- function() {
    s
  }
  # this final function retreieves the cached value of s (if it exists) and returns it
  # finally we assert the names of all four functions so that they can be called 
  list(set = set, get = get,
       setSolve = setSolve, getSolve = getSolve)
}



# the following function uses the "Solve" function to return a matrix
# that is the  inverse of the "x" supplied as a parameter in makeCacheMatrix. 
# this function first checks to see if the inverse has
# already been calculted. If it has, it retrieves this calculated
# inverse and returns it. Otherwise, it calculates the
# inverse through the Solve function and sets the value of the inverse "s" in the cache
# through the setSolve function specified in makeCacheMatrix
cacheSolve <- function(x, ...) {
  # first, the function tries to retreive the inverse "s" from the cache 
  # by calling the getSolve function established in makeCacheVector
  s <- x$getSolve()
  
  # next we test -- if s is not a null value (as it would be if no inverse
  # has been calculated) the function lets us know it is retrieving
  # a cached value, and returns this cached value  

  # if s is a null value:
  if(is.null(s)) {
    dat <- x$get()         #function retrieves initial matrix by calling get function 
                           # established in makeCacheMatrix - gives result name dat
    s <- solve(dat,...)    #calculates inverse of dat using solve & assigns it to "s"   
    x$setSolve(s)          # callssetSolve function we established in makeCacheMatrix to cache this newly calculated value of s
    s                     # returns s   
  }  
  # alternatively, if s is NOT a null value (and therefore exists in the cache):
   else {
    message("getting cached data")    #updates us that the function is retrieved cached data for s
    return(s)                   # returns this cached value exits function without further calculation
  }
# first time we run this, s will be created as inverse of supplied matrix. It will then be returned and 
# this inverse will be cached. The second time we run it (as long as we don't run makeCacheMatrix again)
# it will return cached value, and let us know it is doing so. 
# alternatively, if we run makeCacheMatrix cacheSolve on the new inverse matrix we created and cached 
# the CacheSolve should create and cache a new value of s -- equal to the original matrix 
}



mat <- matrix(data=rnorm(100,5), nrow=5, ncol=5) 
> mat
[,1]     [,2]     [,3]     [,4]     [,5]
[1,] 7.120143 4.474180 5.678526 3.442953 5.044687
[2,] 5.014588 3.765301 5.089040 5.270629 4.967942
[3,] 6.483060 5.885416 6.083363 4.879549 4.757886
[4,] 3.963160 5.249549 6.486263 5.514913 5.745792
[5,] 4.646727 5.245794 5.631393 2.719804 4.060814
> mat1 <- makeCacheMatrix(mat)
> mat_inv <- cacheSolve(mat1)
> mat_inv
[,1]       [,2]         [,3]       [,4]       [,5]
[1,] -0.01949751  0.4233116  0.006181942 -0.4991216  0.2053301
[2,]  2.16407863 -5.0270017  2.385376457  3.3552542 -4.0807552
[3,] -4.17986186  8.3006261 -3.700822283 -5.7585513  7.5217906
[4,] -0.92541931  1.4893792 -0.170712743 -0.9275736  0.8400232
[5,]  3.64303590 -6.4990176  2.157983358  4.8438048 -5.7107186
> mat_inv <- cacheSolve(mat1)
getting cached data
[,1]       [,2]         [,3]       [,4]       [,5]
[1,] -0.01949751  0.4233116  0.006181942 -0.4991216  0.2053301
[2,]  2.16407863 -5.0270017  2.385376457  3.3552542 -4.0807552
[3,] -4.17986186  8.3006261 -3.700822283 -5.7585513  7.5217906
[4,] -0.92541931  1.4893792 -0.170712743 -0.9275736  0.8400232
[5,]  3.64303590 -6.4990176  2.157983358  4.8438048 -5.7107186
> 
  
> mat2 <- makeCacheMatrix(mat_inv)
> cacheSolve(mat2)
[,1]     [,2]     [,3]     [,4]     [,5]
[1,] 7.120143 4.474180 5.678526 3.442953 5.044687
[2,] 5.014588 3.765301 5.089040 5.270629 4.967942
[3,] 6.483060 5.885416 6.083363 4.879549 4.757886
[4,] 3.963160 5.249549 6.486263 5.514913 5.745792
[5,] 4.646727 5.245794 5.631393 2.719804 4.060814

6008830911916c534b36ff00d7dc28ea263c883b

