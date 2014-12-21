
## function makeCacheMatrix creates cache object of matrix and its iverse.
## the cache object name is the hash of the matrix
makeCacheMatrix <- function(x = matrix()) {
        require(digest)
        digfn<-sprintf("m_%s.digest",digest(x))         # create unique hash used as name
        invers<-NA                                      # init value if not invertible
        try(invers<-solve(x),silent=TRUE)               # invert, suppress any error
        assign(digfn,list(m=x,i=invers), envir = .GlobalEnv)    # save/cache result as list
        invers     
}

## function cacheSolve: returns inverse of matrix x
## before inverting x lookup cache using searching hash object of x
cacheSolve <- function(x, ...) {
        require(digest)
        digfn<-sprintf("m_%s.digest",digest(x))         # create unique hash used as var name
        if (exists(digfn)) {
                invers<-get(digfn)$i                    # get result / was already inverted
        } else {
                invers<-makeCacheMatrix(x)              # calculate result and cache
        }
        invers                                          # return value m inverted
}
