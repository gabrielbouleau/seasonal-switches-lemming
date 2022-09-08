### À commenter : 

### Trouver l'intersection entre deux courbes pour le graph de phase


### Crédit à Amaël pour la solution analytique

find_intersect <- function(x1, x2, y1, y2, k){
  
  x1 = log(x1)
  x2 = log(x2)
  y1 = log(y1)
  y2 = log(y2)

  f = function(x, k)
    return(1/x^k)
  
  getAB = function(x1, x2, y1, y2)
    return(list(a = (y2 - y1)/(x2 - x1), b = y1 - (y2 - y1)/(x2 - x1)*x1))
  
  ab_values = getAB(x1, x2, y1, y2)
  
  a = ab_values$a
  b = ab_values$b
  
  if (a + k == 0)
    stop("Error: a + k cannot be 0")
  
  if (-b/(a + k) < 0)
    stop("Error, cannot be negative")
  
  x_sol = exp(b/-(k+a))
  y_sol = f(x_sol, k)
  
  return(c(x_sol = x_sol, y_sol = y_sol, a = a, b = b))
}


find_intersect_B <- function(x1, x2, y1, y2, k, Lhat){
  
  ## First function  g(x) -- B ~ R
  g = function(x, k, Lhat)
    return((k*log(x))/Lhat)
  
  kL = k/Lhat
  
  ## Second function f(x) -- diagonal
  x1 <- 1
  x2 <- 10
  y1 <- 5
  y2 <- 0
  
  getAB = function(x1, x2, y1, y2)
    return(list(a = (y2 - y1)/(x2 - x1), b = y1 - (y2 - y1)/(x2 - x1)*x1))
  
  ab_values = getAB(x1, x2, y1, y2)
  
  a = ab_values$a
  b = ab_values$b  
  
  x_sol = (-kL/a)*VGAM::lambertW((-a/kL)*exp(b/kL))
  y_sol = g(x_sol, k, Lhat)
  
  return(c(x_sol = x_sol, y_sol = y_sol, a = a, b = b))
  
}



## Solve f(x) = g(x)
## a*x + b = K * log(x)
## 0 = -a*x + k*log(x) - b
## 0 = -a*x + k*log((-a/k)*x) - b - k*log(-a/k)
## 0 = (-a/k)*x + log((-a/k)*x) - b/k - log(-a/k)
## b/k + log(-a/k) = (-a/k)*x + log((-a/k)*x)
## exp(b/k + log(-a/k)) = exp((-a/k)*x + log((-a/k)*x))
## exp(b/k)*exp(log(-a/k)) = exp((-a/k)*x)*exp(log((-a/k)*x))
## (-a/k)*exp(b/k) = (-a/k)*x)*exp((-a/k)*x)
## W((-a/k)*exp(b/k)) = (-a/k)*x)
## x = (-k/a)*W((-a/k)*exp(b/k))
## https://math.stackexchange.com/questions/433717/how-to-solve-equations-with-logarithms-like-this-ax-b-logx-c-0
