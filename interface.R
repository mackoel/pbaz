get_gt_snp <- function(conn, ofl, gt_like = '%', gt_limit = -1, snp_like = '%', snp_limit = -1, snp_quality = -1) {
    ret <- 0
    #FIXME check input for errors
    gt_limit = as.numeric(gt_limit)
    snp_limit = as.numeric(snp_limit)
    snp_quality = as.numeric(snp_quality)
    #FIXME sanitize input
    # do job
    SQL <- paste0("SELECT gname FROM genotype WHERE gname LIKE '", gt_like, "'")
    if (gt_limit > 0) {
	SQL <- paste0(SQL, " LIMIT ", gt_limit)
    }
    ret.gt.df <- dbGetQuery(conn, SQL)
    #FIXME check input for errors
    SQL <- paste0("SELECT snpabbr FROM snp WHERE snpabbr LIKE '", snp_like, "'")
    if (snp_quality > 0) {
	SQL <- paste0(SQL, " AND snp.quality > ", snp_quality)
    }
    if (snp_limit > 0) {
	SQL <- paste0(SQL, " LIMIT ", snp_limit)
    }
    ret.snp.df <- dbGetQuery(conn, SQL)
    n_rows <- length(ret.gt.df$gname)
    n_cols <- length(ret.snp.df$snpabbr)
    if (n_rows <= 0) {
	return(3)
    }
    if (n_cols <= 0) {
	return(4)
    }
    ret_data <- do.call("rbind", lapply(1:n_rows,
		FUN = function(rr) {
		    g = ret.gt.df$gname[rr]
		    ret.row <- do.call("cbind", lapply(1:n_cols,
			FUN = function(cc) {
				snp = ret.snp.df$snpabbr[cc]
				qus <- paste0("SELECT ndom FROM gt_snp WHERE gt = '", g, "' AND snpabbr = '", snp, "'")
				ret.ndom <- dbGetQuery(conn, qus)
				if (dim(ret.ndom)[1] > 0) {
				    return(as.numeric(ret.ndom[1,1]))
				} else {
				    return(NA)
				}
			}
		    ))
		    return(ret.row)
		}
	    ))
    ret.df <- data.frame(ret_data, row.names = ret.gt.df$gname)
    colnames(ret.df) <- ret.snp.df$snpabbr
    write.csv(ret.df, file = ofl, row.names = TRUE)
    return(ret)
}
