## Transform VCF data to SFS (dadi format)
# The SNPs must be biallelic.
# f.popmap: popmap file; integers for the second column, pop IDs.
# pops: integers; IDs (same as in popmap) of pops to be included in the SFS.
# filter.indi: integer; when the number of missing values is lager than this, 
#	the individual will be filtered out; when NA, no filtering.
# filter.snp: integer; when the number of missing values is lager than this, 
#	the SNP will be filtered out; when NA, no filtering.
# n.digit: number of digits to keep when rounding up the allele frequencies; 
#	VERY IMPORTANT!!!
vcf2dadi<-function(f.vcf, f.popmap, f.output, pops,
		ploidy=2, n.digit=4,
		filter.indi=NA, filter.snp=NA)
{
	vcf<-as.matrix(read.table(f.vcf,sep="\t",stringsAsFactors=F)[,-c(3:9)])
	snpid<-paste(vcf[,1],as.integer(vcf[,2]),sep="_")
	vcf<-vcf[,-c(1:2)]
	
	# Choose populations
	popmap<-read.table(f.popmap,sep="\t",stringsAsFactors=F)[,2]
	index<-sapply(pops,function(x){which(popmap==x)})
	index<-sort(unlist(index))
	popmap<-popmap[index]
	vcf<-vcf[,index]
	
	# Parse genotypes
	nrow.vcf<-nrow(vcf)
	ncol.vcf<-ncol(vcf)
	chrom1<-matrix(substring(vcf,1,1),nrow.vcf,ncol.vcf)
	chrom2<-matrix(substring(vcf,3,3),nrow.vcf,ncol.vcf)
	
	# Filter individuals according to missing values
	if(!is.na(filter.indi))
	{
		missingVal<-chrom1=="."
		delete.indi<-which(colSums(missingVal)>filter.indi)
		if(length(delete.indi)>0)
		{
			chrom1<-chrom1[,-delete.indi]
			chrom2<-chrom2[,-delete.indi]
			popmap<-popmap[-delete.indi]
		}
	}
	
	# Filter SNPs according to missing values
	if(!is.na(filter.snp))
	{
		missingVal<-chrom1=="."
		delete.snp<-which(rowSums(missingVal)>filter.snp)
		if(length(delete.snp)>0)
		{
			chrom1<-chrom1[-delete.snp,]
			chrom2<-chrom2[-delete.snp,]
			snpid<-snpid[-delete.snp]
		}
	}
	
	nrow.vcf<-nrow(chrom1)
	ncol.vcf<-ncol(chrom1)
	
	n.pop<-length(pops)
	
	# Calculate alternative allele frequencies
	freq<-matrix(0,nrow.vcf,n.pop)
	colnames(freq)<-paste("Pop",pops,sep="_")
	rownames(freq)<-snpid
	for(i in 1:n.pop)
	{
		index<-which(popmap==pops[i])
		sub.chrom1<-chrom1[,index]
		sub.chrom2<-chrom2[,index]
		ref<-rowSums(sub.chrom1=="0")+rowSums(sub.chrom2=="0")
		alt<-rowSums(sub.chrom1=="1")+rowSums(sub.chrom2=="1")
		freq[,i]<-alt/(ref+alt)
	}
	
	# creating a list containing the allele frequencies of the target populations
	data<-vector("list",n.pop)
	for(i in 1:n.pop)
	{
		data[[i]]<-round(as.numeric(freq[,i]),n.digit)	## n.digit!!! Assigning allele frequencies for the ith population
		names(data[[i]])<-snpid	# the names are very important!!
	}
	names(data)<-pops	# these names are also very important; have to be numerics
	
	sampleSizes<-sapply(pops,function(x){sum(popmap==x)})
	
	write.jafs(data,f.output,sampleSizes,ploidy,n.digit)
}
