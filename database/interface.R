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

get_gt_cnum <- function(conn, ofl, gt_like = '%', gt_limit = -1, cn_like = '%', cn_limit = -1) {
    ret <- 0
    #FIXME check input for errors
    gt_limit = as.numeric(gt_limit)
    cn_limit = as.numeric(cn_limit)
    #FIXME sanitize input
    # do job
    SQL <- paste0("SELECT gname FROM genotype WHERE gname LIKE '", gt_like, "'")
    if (gt_limit > 0) {
	SQL <- paste0(SQL, " LIMIT ", gt_limit)
    }
    ret.gt.df <- dbGetQuery(conn, SQL)
    #FIXME check input for errors
    SQL <- paste0("SELECT cnabbr FROM cnum WHERE cnabbr LIKE '", cn_like, "'")
    if (cn_limit > 0) {
	SQL <- paste0(SQL, " LIMIT ", cn_limit)
    }
    ret.cn.df <- dbGetQuery(conn, SQL)
    n_rows <- length(ret.gt.df$gname)
    n_cols <- length(ret.cn.df$cnabbr)
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
				cn = ret.cn.df$cnabbr[cc]
				qus <- paste0("SELECT cnumber FROM gt_cnum WHERE gt = '", g, "' AND cnabbr = '", cn, "'")
				ret.cnum <- dbGetQuery(conn, qus)
				if (dim(ret.cnum)[1] > 0) {
				    return(as.numeric(ret.cnum[1,1]))
				} else {
				    return(NA)
				}
			}
		    ))
		    return(ret.row)
		}
	    ))
    ret.df <- data.frame(ret_data, row.names = ret.gt.df$gname)
    colnames(ret.df) <- ret.cn.df$cnabbr
    write.csv(ret.df, file = ofl, row.names = TRUE)
    return(ret)
}

library(h5)

cptb <-dbReadTable(conn, 'clim_pheno_full')
write.csv(cptb[order(cptb$num),], file = 'supertab.csv', row.names = FALSE)

# sowing-flowering

var_index <- c(39:51, 78:107, 171:201)
res_index <- 11

p_index <- !is.na(tb[,res_index])

dfile <- "chickpea-turkey-kos.h5"
file.remove(dfile)
file <- h5file(dfile)

subindex <- cptb$origin == "Турция"

file["data"] <- as.matrix(cptb[subindex & p_index,var_index])
file["response"] <- as.matrix(cptb[subindex & p_index,res_index])
file["measurements"] <- colnames(cptb[subindex & p_index,var_index])
file["species"] <- as.character(cptb[subindex & p_index,]$variety)

h5close(file)

dfile <- "chickpea-ethiopia-kos.h5"
file.remove(dfile)
file <- h5file(dfile)

subindex <- cptb$origin == "Эфиопия"

file["data"] <- as.matrix(cptb[subindex & p_index,var_index])
file["response"] <- as.matrix(cptb[subindex & p_index,res_index])
file["measurements"] <- colnames(cptb[subindex & p_index,var_index])
file["species"] <- as.character(cptb[subindex & p_index,]$variety)

h5close(file)


#for (i in 7:59) {
#	mn <- mean(cropData[,i], na.rm = TRUE)
#	cropData[is.na(cropData[,i]), i] <- mn
#}

# shoots-flowering
#var_index <- c(13, 20, 26:31, 37:42, 43:52, 54:59)
#res_index <- 8

file["shootsFlowering"]["data"] <- as.matrix(cropData[,var_index])
file["shootsFlowering"]["response"] <- as.matrix(cropData[,res_index])
file["shootsFlowering"]["measurements"] <- colnames(cropData[,var_index])
file["shootsFlowering"]["species"] <- as.character(cropData$Specie)

#
#[11] "avgTempSA"                   "sumTempAF"
#[13] "avgTempAF"                   "sumTempSF"
#[15] "avgTempSF"                   "precipitationSA"
#[17] "precipitationAF"             "precipitationSF"
#[19] "avgPrecipitationSA"          "avgPrecipitationAF"
#[21] "avgPrecipitationSF"          "gtkAF"
#[23] "tempSowing"                  "avgTemp5AfterSowing"
#[25] "avgTemp10AfterSowing"        "avgTemp15AfterSowing"
#[27] "avgTemp20AfterSowing"        "avgTemp30AfterSowing"
#[29] "avgTemp40AfterSowing"        "avgTemp50AfterSowing"
#[31] "avgTemp60AfterSowing"        "avgPrec5BeforeSowing"
#[33] "avgPrec5AfterSowing"         "avgPrec10AfterSowing"
#[35] "avgPrec5Before5AfterSowing"  "avgPrec5Before10AfterSowing"
#[37] "avgPrec15AfterSowing"        "avgPrec20AfterSowing"
#[39] "avgPrec30AfterSowing"        "avgPrec40AfterSowing"
#[41] "avgPrec50AfterSowing"        "avgPrec60AfterSowing"
#[43] "tempShoots"                  "avgTemp30AfterShoots"
#[45] "avgTemp40AfterShoots"        "avgTemp50AfterShoots"
#[47] "avgTemp60AfterShoots"        "avgPrec30AfterShoots"
#[49] "avgPrec40AfterShoots"        "avgPrec50AfterShoots"
#[51] "avgPrec60AfterShoots"        "dayLengthShoots"
#[53] "dayLengthFlowering"          "avgDayLengthAF"
#[55] "avgDayLength10AfterShoots"   "avgDayLength20AfterShoots"
#[57] "avgDayLength30AfterShoots"   "avgDayLength40AfterShoots"
#[59] "avgDayLength50AfterShoots"

# sowing-shoots
var_index <- c(11, 19, 23:25, 32:36)
res_index <- 7

file["data"] <- as.matrix(cropData[,var_index])
file["response"] <- as.matrix(cropData[,res_index])
file["measurements"] <- colnames(cropData[,var_index])
file["species"] <- as.character(cropData$Specie)

h5close(file)

file.remove(dfile)
export LD_LIBRARY_PATH=/home/nilmbb/kkozlov/lib:/home/nilmbb/kkozlov/lib64:$LD_LIBRARY_PATH
module load library/atlas/3.10.3/gcc library/boost/1.64.0 library/openblas/0.2.19/gcc
srun -J s4 -N 1 /home/nilmbb/kkozlov/bin/deepmethod --default-name=chickpea-turkey-intersept-pos-003.ini
