# Internal functions of diagonal_check_by_density
# @keywords internal

unsaturated.diag = function(n.row, n.col, by.density){
	M=matrix(0, nrow=n.row, ncol=n.col)
	ind.M=row(M)+col(M)-1
	tmp=M[ind.M==round(median(ind.M))]
	n.check=by.density*n.row*n.col
	ind.check=round(seq(from=1, to=length(tmp), length.out=n.check))
	tmp[ind.check]=1
	M[ind.M==round(median(ind.M))]=tmp
	M.out=density_check(M, by.density)
	return(M.out)
	}


saturated.diag = function(n.row, n.col, by.density, spread.scale=50*max(n.row, n.col)){
	m=matrix(0, nrow=n.row, ncol=n.col)
	n.diag=n.row+n.col-1
	n.long.diag=abs(nrow(m)-ncol(m))+1
	ind.long.diag=((n.diag-n.long.diag)/2+1):((n.diag-n.long.diag)/2+n.long.diag)
	n.check=by.density*n.row*n.col
	ind.M=row(m)+col(m)-1
	dist.diag=spread.diag(m, ind.M, n.check, n.diag, ind.long.diag, spread.scale)
	m.out=spread.line(m, ind.M, dist.diag)
	out.raw=diag.verify(m.out, ind.M, ind.long.diag, n.check)
	out=density_check(out.raw, by.density)
	return(out)
	}

diag.verify = function(result.matrix, ind.M, ind.long.diag, n.check){
	if(sum(result.matrix)==0){
		mid.long.diag=round(median(ind.long.diag))
		tmp=result.matrix[ind.M==mid.long.diag]
		tmp[round(seq.int(from=1, to=length(tmp), length.out=n.check))]=1
		result.matrix[ind.M==mid.long.diag]=tmp
		return(result.matrix)
	} else { return(result.matrix) }
	}

spread.line = function(M, ind.M, dist.check){
	M.out=M
	diag.vector=split(M, ind.M)
	lapply(1:length(diag.vector), function(x){
		tmp=1:length(diag.vector[[x]])
		tmp.dta=data.frame(in.vector=rep(0, length(tmp))
						, spread.vector=c(rev(tmp[tmp%%2==1]), tmp[tmp%%2==0])
						)
		tmp.check=round(seq.int(from=1, to=length(tmp), length.out=dist.check[x]))
		tmp.dta[tmp.dta$spread.vector%in%tmp.check, 'in.vector']=1
		M.out[ind.M==x]<<-tmp.dta$in.vector
		})
	return(M.out)
	}


spread.diag = function(M, ind.M, n.check, n.diag, ind.long.diag, spread.scale){
#	den.raw=do.call(rbind, lapply(split(M, ind.M), length))/(nrow(M)*ncol(M))
	den.raw=dcauchy(1:n.diag, location=median(ind.long.diag), scale=spread.scale)
	den.std=den.raw/sum(den.raw)
	n.check.diag=round(den.std*n.check)
	return(as.vector(n.check.diag))
	}

