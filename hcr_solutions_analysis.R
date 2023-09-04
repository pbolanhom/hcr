



colors=c(colfunc1(n),colfunc2(n))
OUT=run(tmax=500,legend = F,table=TRUE,timeplot = T, tstep=0.01)

timet=OUT[,1]
Si.dy=OUT[,c(2:(n+1))] #Susceptible class dynamics
Si.eq=matrix(as.numeric(tail(Si.dy)[5,])) 
Si.eq=ifelse(Si.eq<0,0,Si.eq) #Susceptible class equilibrium
Si.eq=ifelse(Si.eq<0.000001,0,Si.eq)
S.dy=rowSums(Si.dy)
S.eq=sum(Si.eq)
hi.dy=Si.dy/S.dy
hi.eq=Si.eq/S.eq

Ii.dy=OUT[,c((n+2):(2*n+1))] #Infected class dynamics
Ii.eq=matrix(as.numeric(tail(Ii.dy)[5,]))
Ii.eq=ifelse(Ii.eq<0,0,Ii.eq) #Infected class equilibrium
Ii.eq=ifelse(Ii.eq<0.000001,0,Ii.eq)
I.dy=rowSums(Ii.dy)
I.eq=sum(Ii.eq)
qi.dy=Ii.dy/I.dy
qi.eq=Ii.eq/I.eq

Ni.dy=Si.dy+Ii.dy #Overall population dynamics
Ni.eq=Si.eq+Ii.eq #Overall population equilibrium

N.dy=S.dy+I.dy #Overall population dynamics
N.eq=S.eq+I.eq #Overall population equilibrium
fi.dy=Ni.dy/N.dy
fi.eq=Ni.eq/N.eq

pi.dy=Ii.dy/Ni.dy
pi.eq=Ii.eq/Ni.eq
p.dy=I.dy/N.dy
p.eq=I.eq/N.eq

### analytical approximations
r=b-d
K=r/l
beta=tau*rowSums(C)
Deltai=beta^2*(r-alpha)^2+2*beta*alpha*l*(b+d+alpha+2*gamma)+alpha^2*l^2
pi.calc=(beta*(r+alpha)-l*alpha-sqrt(Deltai))/(2*alpha*beta)  
Ni.calc=K-pi.calc*alpha/l
Si.calc=matrix((d+l*Ni.calc+alpha+gamma)/beta)
Ii.calc=(Si.calc*(beta-l)-(d+alpha+gamma))/l

zi.eq=(-a1+t1*(A%*%Si.eq))/(2*a2)
zi.calc=(-a1+t1*(A%*%Si.calc))/(2*a2)

qi.calc=Ii.calc/sum(Ii.calc)
cbind(pi.eq,pi.calc)

#Analysis
Fit=(tau*(A%*%Si.eq))-(d+l*Ni.eq+alpha+gamma) 
Fit.calc=(tau*(A%*%Si.calc))-(d+l*Ni.calc+alpha+gamma) 
V(net)$SourceSink=round(Fit,8)>=0  
Fi=NULL
Fi0=NULL
zz=seq(0.01,50,0.1)
for (i in 1:length(Si.eq)) {
  Fi0=(t0+t1*zz)*as.numeric(A[i,]%*%Si.eq)-(d+l*Ni.eq[i]+a1*zz+a2*zz^2+gamma)
  Fi=cbind(Fi,Fi0)
}
Fi=as.data.frame(Fi)
colnames(Fi)=seq(1,n,1)


#FOR SS
zi.eq=(-a1+t1*(A%*%Si.eq))/(2*a2)
zi.calc=(-a1+t1*(A%*%Si.calc))/(2*a2)
db=data.frame("i"=V(net)$Sp,"ki"=matrix(Outdeg_ba2),"Cji's"=matrix(rowSums(C)),
                     "Ssimu"=Si.eq,"Scalc"=Si.calc,Ii.eq,Ii.calc,"qisim"=qi.eq,"qicalc"=qi.calc,
                     "Fisim"=Fit,"Ficalc"=Fit.calc,zi.eq,zi.calc)

  
  
  