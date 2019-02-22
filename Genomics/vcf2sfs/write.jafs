
## Generate a joint allele frequency spectrum (dadi format) from a list.
# data: list object; the name of each element is a numeric value as population ID;
#	each element is a numeric vector containing the alternative allele frequencies (p) of a population;
#	the name of each element in the vector is SNP ID.
# file: character value; output file name.
# sampleSizes: numeric vector; the number of individuals in each population.
# ploidy: numeric value; the ploidy level of the individuals.
# n.digit: number of effective digits to keep after decimal; very important!!!
write.jafs<-function(data,file,sampleSizes,ploidy,n.digit)
{
	pops=as.numeric(names(data))
	n.pop=length(pops)
	n.bin=sampleSizes*ploidy+1
	
	zz<-file(file,"w")
	writeLines(as.character(n.bin),zz,sep=" ")
	writeLines("",zz)
	
	recur<-function(data,pops,n.bin,n.digit)
	{
		n.pop=length(pops)
		n.bin1=n.bin[1]
		bound=round(c(-1,seq(0,1,1/(n.bin1-1))),n.digit)	## n.digit!!! Setting boundaries for the bins
		temp=data[[1]]
		if(length(pops)>1)
		{
			for(i.bin in 1:n.bin1)
			{
				index=names(temp)[(temp<=bound[i.bin+1])==(temp>bound[i.bin])]
				newdata=vector("list",n.pop-1)
				for(i in 2:n.pop) newdata[[i-1]]=na.omit(data[[i]][index])
				recur(newdata,pops[-1],n.bin[-1],n.digit)
			}
		} else
		{
			for(i.bin in 1:n.bin1)
			{
				writeLines(as.character(sum((temp<=bound[i.bin+1])==(temp>bound[i.bin]))),zz,sep=" ")
			}
		}
	}
	
	recur(data,pops,n.bin,n.digit)
	close(zz)
}
