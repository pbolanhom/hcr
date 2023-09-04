

#Relationship between power of preferential attachment (x-axis) and degree variance (y-axis)
#for the Barabasi-Albeirt model
#library(igraph)
#generate graphs
n=15
E=15
links=as.vector(rbind(seq(1,n,1),seq(1,n,1)))#as.vector(c(1,2,1,3,3,2,3,1,5,5))
net0=graph_from_adjacency_matrix(as.matrix(as_adjacency_matrix((sample_pa(n=n, power=1, m=E,
directed=F,out.pref=T))%>%add_edges(links))),weighted=TRUE,mode=c("directed"))
V(net0)$Sp=seq(1,n,1) #adding species id to each node
A0=as.matrix(as_adjacency_matrix(net0,attr="weight"))/degree(net0, mode="out",loops=T) #Comment # to incorporate contact dilution
A0[is.nan(A0)] = 0
net=graph_from_adjacency_matrix(A0,weighted=TRUE,mode=c("directed"))
A=as.matrix(as_adjacency_matrix(net, attr="weight"))
par(mar=c(5,5,2,1)+0,mgp=c(3,1,0),cex=0.75,xpd=F,las=1)
plot(net, vertex.size=17, vertex.label=V(net)$Sp,edge.color="azure4",   
     vertex.label.font=1.2,vertex.label.cex=1,vertex.label.degree=pi/2,margin=.5,edge.arrow.size=.4,vertex.label.dist=0,edge.width=E(net)$weight,edge.label=E(net)$weight,vertex.color="lightblue",
     vertex.label.color="red",layout=layout_with_fr(net),edge.arrow.width=1.5)
#get the variance
degvar=var(degree(net))
#store in a dataframe
#degDATA=NULL
deg.data0=data.frame(E,degvar)
#degDATA=rbind(degDATA,deg.data0)
#plot
par(mar=c(2,2,1,1)+0.1,mgp=c(1,1,0),cex=2,xpd=FALSE,las=1)
plot(degDATA$E,degDATA$degvar,ylim=c(0,40),xlim=c(1,15),pch=19,ylab="",xlab="")
lines(predict(lm(degDATA$degvar~log(degDATA$E))),col='red',lwd=2)
