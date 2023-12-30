
install.packages("nleqslv")
library(nleqslv)


y = function(y1){
  eq = 0.02*exp(-0.05*y1) + 1.02*exp(-y1) - 0.99
  eq
}

soln1 = nleqslv(0, y)

z = function(y2){
  eq = 0.02*exp(-0.5*soln1$x) + 0.02*exp(-soln1$x) + 0.02*exp(-1.5*y2) + 1.02*exp(-2*y2) - 0.97
  eq
}

soln2 = nleqslv(0,z)