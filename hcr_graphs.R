

#Graph for panel SourceSink
sp=seq(1,n,1)
n=8
loopsS=c(rbind(seq(1,n,1),seq(1,n,1)))
links=as.vector(c(loopsS,1,8,2,8,3,8,4,7,5,6,6,7,7,8)) #arbitrary
net0=graph_from_adjacency_matrix(
  as.matrix(as_adjacency_matrix(
    (sample_pa(n=n, power=1, m=0,directed=F,out.pref=T))%>%add_edges(links))),weighted=TRUE,mode=c("directed"))
V(net0)$Sp=seq(1,n,1) #adding species id to each node
A0=as.matrix(as_adjacency_matrix(net0,attr="weight"))/degree(net0, mode="out",loops=T) #Comment # to incorporate contact dilution
A0[is.nan(A0)] = 0
net=graph_from_adjacency_matrix(A0,weighted=TRUE,mode=c("directed"))
V(net)$Sp=sp
A=as.matrix(as_adjacency_matrix(net, attr="weight"))
plot(net,vertex.label="", vertex.size=10, vertex.color="orange",
     vertex.label.font=1.2,vertex.label.cex=0.7,vertex.label.degree=pi/2,
     vertex.label.dist=2, vertex.label.color="black",edge.color="azure4",
     edge.width=E(net)$weight,edge.arrow.size=.4,edge.arrow.width=1.5,margin=.5,
     layout=layout_with_fr(net),rescale=T)
degr=degree(net,mode="out")


#Graph for Panel FIT 
n=22
loops=as.vector(rbind(seq(1,n,1),seq(1,n,1)))
sp=seq(1,n,1)
#Graph a) - homogeneous
homo_undirected=connect.neighborhood(make_ring(n) , 8)

homo0=graph_from_adjacency_matrix(
  as.matrix(
    as_adjacency_matrix(
      (homo_undirected)%>%add_edges(loops))),
  weighted=TRUE,mode=c("directed"))

AH0=as.matrix(as_adjacency_matrix(homo0,attr="weight"))/degree(homo0, mode="out",loops=T)
AH0[is.nan(AH0)] = 0
HOMO=graph_from_adjacency_matrix(AH0,weighted=TRUE,mode=c("directed"))
V(HOMO)$Sp=sp
AH=as.matrix(as_adjacency_matrix(HOMO, attr="weight"))
plot(HOMO,vertex.label="", vertex.size=10, vertex.color="orange",
     vertex.label.font=1.2,vertex.label.cex=0.7,vertex.label.degree=pi/2,
     vertex.label.dist=2, vertex.label.color="black",edge.color="azure4",
     edge.width=E(HOMO)$weight,edge.arrow.size=.4,edge.arrow.width=1.5,margin=.5,
     layout=layout_in_circle(HOMO),rescale=T)
Outdeg_homo=degree(HOMO,mode="out")
Maxentr=Entropy(Outdeg_homo)
shannba_homo=diversity(Outdeg_homo)
edgeTotal_homo=sum(Outdeg_homo)
edgeTotal_homo
deg_diff.homo=vapply(seq_along(Outdeg_homo), FUN = function(i, X) X^(-1/2)-X[[i]]^(-1/2), 
                    FUN.VALUE = numeric(length(Outdeg_homo)), Outdeg_homo) 
Irreg.ij.homo=(deg_diff.homo)^2
Irreg.i.homo=rowSums(Irreg.ij.homo)
HOMO_Heterogeneity=sum(Irreg.i.homo)

#
#Graph b)
ba1_undirected=sample_pa(n=n, power=1, m=11, directed=F,out.pref=T)
ba10=graph_from_adjacency_matrix(
  as.matrix(
    as_adjacency_matrix(
      (ba1_undirected)%>%add_edges(loops))),
  weighted=TRUE,mode=c("directed"))
Aba10=as.matrix(as_adjacency_matrix(ba10,attr="weight"))/degree(ba10, mode="out",loops=T)
Aba10[is.nan(Aba10)] = 0
ba1=graph_from_adjacency_matrix(Aba10,weighted=TRUE,mode=c("directed"))
V(ba1)$Sp=sp
Aba1=as.matrix(as_adjacency_matrix(ba1, attr="weight"))
plot(ba1,vertex.label="", vertex.size=10, vertex.color="orange",
     vertex.label.font=1.2,vertex.label.cex=0.7,vertex.label.degree=pi/2,
     vertex.label.dist=2, vertex.label.color="black",edge.color="azure4",
     edge.width=E(ba1)$weight,edge.arrow.size=.4,edge.arrow.width=1.5,margin=.5,
     layout=layout_with_fr(ba1),rescale=T)
Outdeg_ba1=degree(ba1,mode="out")
shannba1=diversity(Outdeg_ba1)
Heter=Maxentr-Entropy(Outdeg_ba2)
edgeTotal_ba1=sum(Outdeg_ba1)
edgeTotal_ba1
deg_diff.ba1=vapply(seq_along(Outdeg_ba1), FUN = function(i, X) X^(-1/2)-X[[i]]^(-1/2), 
                FUN.VALUE = numeric(length(Outdeg_ba1)), Outdeg_ba1) 
Irreg.ij.ba1=(deg_diff.ba1)^2
Irreg.i.ba1=rowSums(Irreg.ij.ba1)
ba1_Heterogeneity=sum(Irreg.i.ba1)


#Graph c)
ba2_undirected=sample_pa(n=n, power=1, m=6, directed=F,out.pref=T)
ba20=graph_from_adjacency_matrix(
  as.matrix(
    as_adjacency_matrix(
      (ba2_undirected)%>%add_edges(loops))),
  weighted=TRUE,mode=c("directed"))
Aba20=as.matrix(as_adjacency_matrix(ba20,attr="weight"))/degree(ba20, mode="out",loops=T)
Aba20[is.nan(Aba20)] = 0
ba2=graph_from_adjacency_matrix(Aba20,weighted=TRUE,mode=c("directed"))
V(ba2)$Sp=sp
Aba2=as.matrix(as_adjacency_matrix(ba2, attr="weight"))
plot(ba2,vertex.label="", vertex.size=10, vertex.color="orange",
     vertex.label.font=1.2,vertex.label.cex=0.7,vertex.label.degree=pi/2,
     vertex.label.dist=2, vertex.label.color="black",edge.color="azure4",
     edge.width=E(ba2)$weight,edge.arrow.size=.4,edge.arrow.width=1.5,margin=.5,
     layout=layout_with_fr(ba2),rescale=T)
Outdeg_ba2=degree(ba2,mode="out")
shannba2=diversity(Outdeg_ba2)
edgeTotal_ba2=sum(Outdeg_ba2)
edgeTotal_ba2
deg_diff.ba2=vapply(seq_along(Outdeg_ba2), FUN = function(i, X) X^(-1/2)-X[[i]]^(-1/2), 
                    FUN.VALUE = numeric(length(Outdeg_ba2)), Outdeg_ba2) 
Irreg.ij.ba2=(deg_diff.ba2)^2
Irreg.i.ba2=rowSums(Irreg.ij.ba2)
ba2_Heterogeneity=sum(Irreg.i.ba2)



#Graph d)
ba3_undirected=sample_pa(n=n, power=1, m=1, directed=F,out.pref=T)
ba30=graph_from_adjacency_matrix(
  as.matrix(
    as_adjacency_matrix(
      (ba3_undirected)%>%add_edges(loops))),
  weighted=TRUE,mode=c("directed"))
Aba30=as.matrix(as_adjacency_matrix(ba30,attr="weight"))/degree(ba30, mode="out",loops=T)
Aba30[is.nan(Aba30)] = 0
ba3=graph_from_adjacency_matrix(Aba30,weighted=TRUE,mode=c("directed"))
V(ba3)$Sp=sp
Aba3=as.matrix(as_adjacency_matrix(ba3, attr="weight"))
plot(ba3,vertex.label="", vertex.size=10, vertex.color="orange",
     vertex.label.font=1.2,vertex.label.cex=0.7,vertex.label.degree=pi/2,
     vertex.label.dist=2, vertex.label.color="black",edge.color="azure4",
     edge.width=E(ba3)$weight,edge.arrow.size=.4,edge.arrow.width=1.5,margin=.5,
     layout=layout_with_fr(ba3),rescale=T)
Outdeg_ba3=degree(ba3,mode="out")
shannba3=diversity(Outdeg_ba3)
edgeTotal_ba3=sum(Outdeg_ba3)
edgeTotal_ba3
deg_diff.ba3=vapply(seq_along(Outdeg_ba3), FUN = function(i, X) X^(-1/2)-X[[i]]^(-1/2), 
                    FUN.VALUE = numeric(length(Outdeg_ba3)), Outdeg_ba3) 
Irreg.ij.ba3=(deg_diff.ba3)^2
Irreg.i.ba3=rowSums(Irreg.ij.ba3)
ba3_Heterogeneity=sum(Irreg.i.ba3)


