
## Resampling from a SFS with a fastSimCoal format for the purpose of bootstrapping
# f.fsc: input file (fastSimCoal format)
# n.rep: number of repeats for the bootstrap
# nameroot: nameroot for the output files
fsc.sample<-function(f.fsc,n.rep,nameroot)
{
	fsc<-read.table(f.fsc,header=T,sep="\t",skip=1,row.names=1)
	n.site<-sum(fsc)
	n.row<-nrow(fsc)
	n.col<-ncol(fsc)
	n.entry<-n.row*n.col
	
	lamdas<-unlist(fsc)[-1]
	
	for(i in 1:n.rep)
	{
		temp<-rpois(n.entry-1,lamdas)
		temp<-c(n.site-sum(temp),temp)
		temp<-matrix(temp,n.row,n.col)
		rownames(temp)<-rownames(fsc)
		colnames(temp)<-colnames(fsc)
		f.output<-paste("%s-%0",nchar(n.rep),"d.fsc",sep="")
		f.output<-sprintf(f.output,nameroot,i)
		cat("1 observations\n\t",file=f.output)
		oldw<-getOption("warn")
		options(warn=-1)
		write.table(temp,file=f.output,append=T,
			sep="\t",col.names=T,row.names=T,quote=F)
		options(warn=oldw)
	}
}
