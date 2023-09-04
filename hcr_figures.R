

#SS
par(mar=c(5,5,2,1)+0,mgp=c(3,1,0),cex=0.75,xpd=F,las=1)
plot(netSS,vertex.label=V(netSS)$Sp, vertex.size=15,
     vertex.color=c("gray80","gray30")[1+(V(netSS)$SourceSink=="TRUE")],
     vertex.label.font=2,vertex.label.cex=1.7,vertex.label.degree=pi/2, 
     vertex.label.dist=2, vertex.label.color="black",edge.color="azure4",
     edge.width=E(netSS)$weight,edge.arrow.size=.4,edge.arrow.width=1.5,margin=.5,
     layout=layout_with_fr(netSS),rescale=T)
par(mar=c(2,2,1,1)+0.1,mgp=c(1,1,0),cex=2,xpd=FALSE,las=1)
matplot(x=zz,y=FiSS,type = "l",lwd=4,lty=c(1), #plotting the landscape
        xlab="",ylab="",#expression("F"[i](z)),
        frame=T,ylim=c(-2,4),xlim=c(0,12),col=c("gray70","gray15")[1+(V(netSS)$SourceSink=="TRUE")])
matpoints(x=dbSS$zi.eq,y=matrix(apply(FiSS,2,max)),pch=19)
abline(h=0,col="black",lty=2,lwd=1) #Fi=0
abline(v=z,col="black",lty=1,lwd=2)

round(dbSS[,c(1,2,3,4,5,9,10)],3)


#FIT
#a)
par(mar=c(5,5,2,1)+0,mgp=c(3,1,0),cex=0.75,xpd=F,las=1)
plot(netFITa,vertex.label="", vertex.size=10,
     vertex.color=c("gray80","gray30")[1+(V(netFITa)$SourceSink=="TRUE")],
     vertex.label.font=2,vertex.label.cex=1.2,vertex.label.degree=pi/2, 
     vertex.label.dist=2, vertex.label.color="black",edge.color="azure4",
     edge.width=E(netFITa)$weight,edge.arrow.size=.4,edge.arrow.width=1.5,margin=.5,
     layout=layout_in_circle(netFITa),rescale=T)
par(mar=c(2,2,1,1)+0.1,mgp=c(1,1,0),cex=2,xpd=FALSE,las=1)
matplot(x=zz,y=FiFITa,type = "l",lwd=4,lty=c(1), #plotting the landscape
        xlab="",ylab="",#expression("F"[i](z)),
        frame=T,ylim=c(-2,4),xlim=c(0,12),col=c("gray70","gray15")[1+(V(netFITa)$SourceSink=="TRUE")])
matpoints(x=dbFITa$zi.eq,y=matrix(apply(FiFITa,2,max)),pch=19)
abline(h=0,col="black",lty=2,lwd=1) #Fi=0
abline(v=z,col="black",lty=1,lwd=2)



#b)
par(mar=c(5,5,2,1)+0,mgp=c(3,1,0),cex=0.75,xpd=F,las=1)
plot(netFITb,vertex.label="", vertex.size=10,
     vertex.color=c("gray80","gray30")[1+(V(netFITb)$SourceSink=="TRUE")],
     vertex.label.font=2,vertex.label.cex=1.2,vertex.label.degree=pi/2, 
     vertex.label.dist=2, vertex.label.color="black",edge.color="azure4",
     edge.width=E(netFITb)$weight,edge.arrow.size=.4,edge.arrow.width=1.5,margin=.5,
     layout=layout_with_fr(netFITb),rescale=T)
par(mar=c(2,2,1,1)+0.1,mgp=c(1,1,0),cex=2,xpd=FALSE,las=1)
matplot(x=zz,y=FiFITb,type = "l",lwd=4,lty=c(1), #plotting the landscape
        xlab="",ylab="",#expression("F"[i](z)),
        frame=T,ylim=c(-2,4),xlim=c(0,12),col=c("gray70","gray15")[1+(V(netFITb)$SourceSink=="TRUE")])
matpoints(x=dbFITb$zi.eq,y=matrix(apply(FiFITb,2,max)),pch=19)
abline(h=0,col="black",lty=2,lwd=1) #Fi=0
abline(v=z,col="black",lty=1,lwd=2)



#c)
par(mar=c(5,5,2,1)+0,mgp=c(3,1,0),cex=0.75,xpd=F,las=1)
plot(netFITc,vertex.label="", vertex.size=10,
     vertex.color=c("gray80","gray30")[1+(V(netFITc)$SourceSink=="TRUE")],
     vertex.label.font=2,vertex.label.cex=1.2,vertex.label.degree=pi/2, 
     vertex.label.dist=2, vertex.label.color="black",edge.color="azure4",
     edge.width=E(netFITc)$weight,edge.arrow.size=.4,edge.arrow.width=1.5,margin=.5,
     layout=layout_with_fr(netFITc),rescale=T)
par(mar=c(2,2,1,1)+0.1,mgp=c(1,1,0),cex=2,xpd=FALSE,las=1)
matplot(x=zz,y=FiFITc,type = "l",lwd=4,lty=c(1), #plotting the landscape
        xlab="",ylab="",#expression("F"[i](z)),
        frame=T,ylim=c(-2,4),xlim=c(0,12),col=c("gray70","gray15")[1+(V(netFITc)$SourceSink=="TRUE")])
matpoints(x=dbFITc$zi.eq,y=matrix(apply(FiFITc,2,max)),pch=19)
abline(h=0,col="black",lty=2,lwd=1) #Fi=0
abline(v=z,col="black",lty=1,lwd=2)



#d)
par(mar=c(5,5,2,1)+0,mgp=c(3,1,0),cex=0.75,xpd=F,las=1)
plot(netFITd,vertex.label="", vertex.size=10,
     vertex.color=c("gray80","gray30")[1+(V(netFITd)$SourceSink=="TRUE")],
     vertex.label.font=2,vertex.label.cex=1.2,vertex.label.degree=pi/2, 
     vertex.label.dist=2, vertex.label.color="black",edge.color="azure4",
     edge.width=E(netFITd)$weight,edge.arrow.size=.4,edge.arrow.width=1.5,margin=.5,
     layout=layout_with_fr(netFITd),rescale=T)

par(mar=c(2,2,1,1)+0.1,mgp=c(1,1,0),cex=2,xpd=FALSE,las=1)
matplot(x=zz,y=FiFITd,type = "l",lwd=4,lty=c(1), #plotting the landscape
        xlab="",ylab="",#expression("F"[i](z)),
        frame=T,ylim=c(-2,4),xlim=c(0,12),col=c("gray70","gray15")[1+(V(netFITd)$SourceSink=="TRUE")])
matpoints(x=dbFITd$zi.eq,y=matrix(apply(FiFITd,2,max)),pch=19)
abline(h=0,col="black",lty=2,lwd=1) #Fi=0
abline(v=z,col="black",lty=1,lwd=2)


