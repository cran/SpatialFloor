#' Extract the designed diagonal pattern from the system data file (Internal functions)
#'
#' Functions of extracting the pre-designed diagonal pattern (density from 0.01 up to 0.35)


diag_extract=function(){
	tmp=diag_pattern_1_35
	lapply(1:nrow(tmp), function(i){
		t1=tmp[i,]
		pattern_matrix(n.row=t1$n.row, n.col=t1$n.col, coor.str=t1$coord)
	})
}

pattern_matrix=function(n.row, n.col, coor.str){
	tmp=matrix(nrow=n.row, ncol=n.col, 0)
	ind=strsplit(coor.str, '\\++')[[1]]
	lapply(ind, function(x){
		t.ind=as.numeric(unlist(strsplit(as.character(x), '')))
		if(length(t.ind)==1) t.ind=c(10, t.ind) else t.ind=t.ind
		tmp[ifelse(t.ind[1]==0, 10, t.ind[1]), ifelse(t.ind[2]==0, 10, t.ind[2])] <<- 1
	})
	return(tmp)
}