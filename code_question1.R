#code for question1

install.packages("nleqslv")
library(nleqslv)


seq1 = seq(-0.5,-1,by=-0.5)
seq2 = seq(-0.5,-2,by=-0.5)
seq3 = seq(-0.5,-3,by=-0.5)
seq5 = seq(-0.5,-5,by=-0.5)
seq10 = seq(-0.5,-10,by=-0.5)

prices = c(1.0298, 1.0194, 1.0004, 0.9570, 0.8861)

#computation for yield

f1 = function(y1){
  eq = 0
  vec = c(y1,y1)
  i = 1;
  for(x in seq1) {
      eq = eq + 0.02*exp(x*vec[i])
      i = i+1;
  }
  eq + exp(-y1) - prices[1]
}

obj = nleqslv(1, f1)
y1 = obj$x

f2 = function(y2){
  eq = 0
  vec = c(y1,y1, y2, y2)
  i = 1;
  for(x in seq2) {
    eq = eq + 0.02*exp(x*vec[i])
    i = i+1;
  }
  eq + exp(-2*y2) - prices[2]
}

obj = nleqslv(1,f2)
y2 = obj$x
  
f3 = function(y3){
  eq = 0
  vec = c(y1,y1, y2, y2, y3, y3)
  i = 1;

  for(x in seq3) {
    eq = eq + 0.02*exp(x*vec[i])
    i = i+1;
  }
  eq + exp(-3*y3) - prices[3]
  
}
  
obj = nleqslv(1,f3)
y3 = obj$x
y4 = y3

f5 = function(y5){
  eq = 0
  vec = c(y1,y1, y2, y2, y3, y3, y4, y4, y5, y5)
  i = 1;
  
  for(x in seq5) {
    eq = eq + 0.02*exp(x*vec[i])
    i = i+1;
  }
  eq + exp(-5*y5) - prices[4]
}

obj = nleqslv(1,f5)
y5 = obj$x
y6 = y5
y7 = y5
y8 = y5
y9 = y5

f10 = function(y10){
  eq = 0
  vec = c(y1,y1, y2, y2, y3, y3, y4, y4, y5, y5, y6, y6, y7, y7, y8, y8, y9, y9, y10, y10)
  i = 1;
  
  for(x in seq10) {
    eq = eq + 0.02*exp(x*vec[i])
    i = i+1;
  }
  eq + exp(-10*y10) - prices[5]
}

obj = nleqslv(1, f10)
y10 = obj$x

#plot yields
x = seq(0.5,10, by=0.5)
y = c(y1,y1,y2, y2,y3,y3,y4,y4,y5,y5,y6,y6,y7,y7,y8,y8,y9,y9,y10,y10)
plot(x,y,main="bootstrapped yields", ylab = "y0(T)", xlab = "T", type="p", ylim = c(0.0,0.08),
     pch = 19, col = "blue")
axis(1, at=seq(0.0, 10.0, 1.0))
axis(2, at=seq(0.0, 0.08, 0.01))

# computation for yield to maturity
# should get 1 value for each bond

ytm_f1 = function(y0){
  eq = 0
  i = 1;
  for(x in seq1) {
    eq = eq + 0.02*exp(x*y0)
    i = i+1;
  }
  eq + exp(-y0) - prices[1]
}

obj = nleqslv(1, ytm_f1)
ytm1 = obj$x

ytm_f2 = function(y0){
  eq = 0
  i = 1;
  for(x in seq2) {
    eq = eq + 0.02*exp(x*y0)
    i = i+1;
  }
  eq + exp(-2*y0) - prices[2]
}

obj = nleqslv(1,ytm_f2)
ytm2 = obj$x

ytm_f3 = function(y0){
  eq = 0
  i = 1;
  for(x in seq3) {
    eq = eq + 0.02*exp(x*y0)
    i = i+1;
  }
  eq + exp(-3*y0) - prices[3]
}

obj = nleqslv(1,ytm_f3)
ytm3 = obj$x


ytm_f5 = function(y0){
  eq = 0
  i = 1;
  for(x in seq5) {
    eq = eq + 0.02*exp(x*y0)
    i = i+1;
  }
  eq + exp(-5*y0) - prices[4]
}

obj = nleqslv(1,ytm_f5)
ytm5 = obj$x

ytm_f10 = function(y0){
  eq = 0
  i = 1;
  for(x in seq10) {
    eq = eq + 0.02*exp(x*y0)
    i = i+1;
  }
  eq + exp(-10*y0) - prices[5]
}

obj = nleqslv(1,ytm_f10)
ytm10 = obj$x

# computation for macauly duration
# one for each bond

f_md1 = function(){
  md = 0;
  for(x in seq1){
    md = md + (-x) * exp(x*ytm1) * 0.02 
  }
  md = (md + (-x)*exp(x*ytm1)) / prices[1]
}

md1 = f_md1()


f_md2 = function(){
  md = 0;
  for(x in seq2){
    md = md + (-x) * exp(x*ytm2) * 0.02 
  }
  md = (md + (-x) * exp(x*ytm2)) / prices[2]
}

md2 = f_md2()

f_md3 = function(){
  md = 0;
  for(x in seq3){
    md = md + (-x) * exp(x*ytm3) * 0.02 
  }
  md = (md + (-x) * exp(x*ytm3)) / prices[3]
}

md3 = f_md3()

f_md5 = function(){
  md = 0;
  for(x in seq5){
    md = md + (-x) * exp(x*ytm5) * 0.02 
  }
  md = (md + (-x) * exp(x*ytm5)) / prices[4]
}

md5 = f_md5()

f_md10 = function(){
  md = 0;
  for(x in seq10){
    md = md + (-x) * exp(x*ytm10) * 0.02 
  }
  md = (md + (-x) * exp(x*ytm10)) / prices[5]
}

md10 = f_md10()

#computation for duration
f_d1 = function(){
  eq = 0
  vec = c(y1,y1)
  i = 1;
  for(x in seq1) {
    eq = eq + (-x)*0.02*exp(x*vec[i])
    i = i+1;
  }
  eq = (eq + (-x)*exp(-y1)) / prices[1]
}

d1 = f_d1()

f_d2 = function(){
  eq = 0
  vec = c(y1,y1, y2, y2)
  i = 1;
  for(x in seq2) {
    eq = eq + (-x)*0.02*exp(x*vec[i])
    i = i+1;
  }
  eq = (eq + (-x)*exp(-2*y2)) / prices[2]
}

d2 = f_d2()

f_d3 = function(){
  eq = 0
  vec = c(y1,y1, y2, y2, y3, y3)
  i = 1;
  
  for(x in seq3) {
    eq = eq + (-x)*0.02*exp(x*vec[i])
    i = i+1;
  }
  eq = (eq + (-x)*exp(-3*y3)) / prices[3]
  
}

d3 = f_d3()

f_d5 = function(){
  eq = 0
  vec = c(y1,y1, y2, y2, y3, y3, y4, y4, y5, y5)
  i = 1;
  
  for(x in seq5) {
    eq = eq + (-x)*0.02*exp(x*vec[i])
    i = i+1;
  }
  eq = (eq + (-x)*exp(-5*y5)) / prices[4]
}

d5 = f_d5()

f_d10 = function(){
  eq = 0
  vec = c(y1,y1, y2, y2, y3, y3, y4, y4, y5, y5, y6, y6, y7, y7, y8, y8, y9, y9, y10, y10)
  i = 1;
  
  for(x in seq10) {
    eq = eq + (-x)*0.02*exp(x*vec[i])
    i = i+1;
  }
  eq = (eq + (-x)*exp(-10*y10)) / prices[5]
}

d10 = f_d10()

