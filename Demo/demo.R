library(MRFM)

activation1<-function(x){
  m=x*I(x>0)
  # m=x*I(x>0)
  m
}
activation2<-function(x,alpha=1){
  # m=2*x*I(x>=0)+alpha*(exp(2*x)-1)*I(x<0)
  #m=I(x>0)*x
  # m=x*I(x>0)*3+I(x<0)*(exp(3*x)-1)
  m=I(x>0)*10
  # m=1/(1+exp(-x))
  m
}

activation3<-function(x){
  m=I(x>0)*x*0.1
  # m=exp(3*x)
  m
}

#you can define the activation function by yourself.

P=c(seq(5,950,5),seq(955,1050,5),seq(1075,1600,25))/300


psi=10/3 #n/d

Results=c()
complexity=c()
for (iter in P){
  
#This is N_j/d
  psi1=(iter)
  psi2=(iter)
  psi3=(iter*3)
  complexity=c(complexity,(psi1+psi2+psi3)/psi)
  values=getvalue(lbd=0.0001,psi=10/3,F1=1,tau = 0.1,activation1,activation2,activation3,
                  psi1,psi2,psi3)
  
  Results<-c(Results,mainvalue(values))

}


plot(complexity,Results,type="l")



