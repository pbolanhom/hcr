

model = function(t, state, parms){
  with(as.list(c(state,parms)),{
    S=state[1:n]
    I=state[(n+1):(2*n)]
    N=S+I
    dS=N*b+gamma*I-S*(d+l*N+tau*(C%*%I))
    dI=S*tau*(C%*%I)-I*(d+l*N+alpha+gamma)
    return(list(c(dS,dI)))    
  }) 
}


a1=0.248
a2=0.2
t0=0.004
t1=0.4
z=2.533773
tau=t1*z-t0
alpha=a1*z+a2*z^2
b=2
d=0.2
l=0.15
gamma=0.2

# run depending on the graph you want to analyze:
#for the source-sink panel (figure 2 in the manuscript)
net=net
A=A
#for figure 3 in the manuscript:
# for the homogeneous graph (panel fig3 a))
net=HOMO
A=AH
#fig3 b)
net=ba1
A=Aba1
#fig3 c)
net=ba2
A=Aba2
#fig3 d)
net=ba3
A=Aba3
#incorporate the identity of the nodes
V(net)$Sp=sp
#define matrix C (transpose the adjacency matrix)
C=t(A)

#define the initial conditions
S=rep(0.5,n)  
I=rep(0.5,n)
s=c(S,I) 
p=c()

