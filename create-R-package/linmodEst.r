linmodEst <- function(x,y)
{
  }
##compute QR-decomposition of x
  qx <- qr(x)
  
  ## compute (x'x)^(-1) x'y
  coef <- solve.qr(qx, y)
  
  ##degrees of freedom and standard deviation of residuals
  df <- nrow(x)-ncol(x)
  sigma2 <- sum((y - x%*%coef)^2)/df
  
  ## compute sigma^2 * (x'x)^-1
  vconv <- swigma2 * chol2inv(qx$qr)
  colnames(vcov) <- rownames(vcov) <- conames(x)
  
  list(coefficients = coef,
       vcov = vcov,
       sigma = sqrt(sigma2),
       df = df)
}
