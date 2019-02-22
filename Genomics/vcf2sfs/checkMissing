## Check the distribution of the missing values.
# Help decide whether some individuals or SNPs should be filtered out.
checkMissing<-function(f.vcf,f.popmap,pops)
{
	vcf<-as.matrix(read.table(f.vcf,sep="\t",stringsAsFactors=F)[,-c(3:9)])
	snpid<-paste(vcf[,1],as.integer(vcf[,2]),sep="_")
	vcf<-vcf[,-c(1:2)]
	
	popmap<-read.table(f.popmap,sep="\t",stringsAsFactors=F)[,2]
	index<-sapply(pops,function(x){which(popmap==x)})
	index<-sort(unlist(index))
	popmap<-popmap[index]
	vcf<-vcf[,index]
	
	nrow.vcf<-nrow(vcf)
	ncol.vcf<-ncol(vcf)
	chrom1<-matrix(substring(vcf,1,1),nrow.vcf,ncol.vcf)
	chrom2<-matrix(substring(vcf,3,3),nrow.vcf,ncol.vcf)
	
	missing<-chrom1=="."
	par(mfrow=c(1,3))
	image(missing,
		xlab=paste("SNPs: ",dim(missing)[1],sep=""),
		ylab=paste("Individuals: ",dim(missing)[2],sep=""))
	plot(sort(colSums(missing)),main="By individual",
		xlab="Individuals",ylab="Number of missing values")
	plot(table(rowSums(missing)),main="By SNP",
		xlab="Number of missing values",ylab="Number of SNPs")
}
