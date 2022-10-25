y <- c(2,3,6,7,8,9,10,12,15)
x <- c(-1,-1,0,0,0,0,1,1,1)

df <- as.data.frame(cbind(x,y))

fit <- glm(y ~ x, data = df, family = "poisson")
summary(fit)

f.iwls <- function(d,b,niter=1){
  X <- cbind(1,d[,"x"]) #design matrix
  
  
  bb <- b
  for (i in 1:niter) {
    W <- diag(exp(b[1]+b[2]*d[,"x"])) # W diagonal
    z <- as.matrix((b[1]+b[2]*d[,"x"] + (d[,"y"] / exp(b[1]+ b[2]*d[,"x"])) -1 )) #z vector
    b <- solve(t(X)%*%W%*%X)%*%t(X)%*%W%*%z #MLE
    
    bb <- rbind(bb,t(b))
  }
  return(list(b=b,Information=t(X)%*%W%*%X, bb=bb))
}

f.iwls(d = df, b = c(7,5), niter = 6)
f.iwls(d = df, b = c(1,0), niter = 6)
