## Change from dadi format to fastSimCoal format, and fold
# Only for fastSimCoal (one dimension)
# A plot will be generated as well.
# pop: ID for the population; doesn't have to be the same as in popmap
dadi2fsc.1pop<-function(f.fs,f.output,pop,fold=T)
{
	dimention<-as.integer(read.table(f.fs,nrows=1))
	fs<-scan(f.fs,skip=1,quiet=T)
	
	# format into matrix
	fs<-matrix(fs,1,dimention[1])
	
	# fold (with overall minor allele frequency)
	if(fold)
	{
		fs.turn<-fs[,dimention[1]:1]
		fs<-fs+fs.turn
		index<-(dimention[1]+1)/2
		fs[,index]<-fs[,index]/2
		index<-1:index
		fs[,-index]<-0
	}
	
	# add row names and column names
	colnames(fs)<-paste("d",pop[1],"_",1:dimention[1]-1,sep="")
	
	# output
	cat("1 observations\n",file=f.output)
	oldw<-getOption("warn")
	options(warn=-1)
	write.table(fs,file=f.output,append=T,sep="\t",col.names=T,row.names=F,quote=F)
	options(warn=oldw)
	barplot(as.integer(fs))
}
