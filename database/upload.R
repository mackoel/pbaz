locdf <- data.frame(locname = "KOS",
		country = "Russia",
		region = "Kuban")

SQL <- sqlAppendTable(conn, "location", locdf, row.names = FALSE)
dbSendStatement(conn, SQL)

kos2016meteo <- read.csv('../meteo2016.csv')
mdf <- data.frame(cbind(1:5, rep('KOS', 5), rep(2016, 5), 4:8, t(kos2016meteo[,-1])))
colnames(mdf) <- c('id', 'location', 'year', 'month', 'tmean', 'pmean', 'pdays', 'tsum10', 'tsoil', 'hrel')
SQL <- sqlAppendTable(conn, "meteo", mdf, row.names = FALSE)
dbSendStatement(conn, SQL)

kos2017meteo <- read.csv('../meteo2017.csv')
mdf <- data.frame(cbind(6:10, rep('KOS', 5), rep(2017, 5), 4:8, t(kos2017meteo[,-1])))
colnames(mdf) <- c('id', 'location', 'year', 'month', 'tmean', 'pmean', 'pdays', 'tsum10', 'tsoil', 'hrel')
SQL <- sqlAppendTable(conn, "meteo", mdf, row.names = FALSE)
dbSendStatement(conn, SQL)

pd <- read.csv('../pheno_doug_2017.csv', header = 1, stringsAsFactors = FALSE)

colnames(pd) <- c("X","Y","catnumber","ID","ancright","inseriesnum","origin","sowing","Number.sowing.seeds","Number.of.rows","seedlings10","seedlings75","flowering10","flowering75","maturityStart","maturityFull","Sowing...the.beginning.seedlings.days.","Beginning.seedlings...the.beginning.of.flowering..days.","Beginning.seedlings...full.of.flowering..days.","Beginning.of.flowering...full.of.flowering..days.","Beginning.of.flowering...the.beginning.of.ripening..days.","Beginning.seedlings...the.beginning.of.ripening..days.","Beginning.seedlings...full.maturation..days.","The.beginning.of.ripening...full.maturation..days.","flowerColour","bushShape","Ptht_1","Hlp_1","PPP_1","PDW_1","PDL_1","SPD_1","SCO","TSW","SYDP_1","PDH","NODULS","PodPed","ascDamage")

# SPD_1 - number of seeds per pod
# SYDP_1 - weight.of.seeds.per.plot..g
# PodPed - Number.of.pod.per.Peduncle

pd$seedlings10[grep(",",pd$seedlings10)] <- format.Date(as.Date(pd$seedlings10[grep(",",pd$seedlings10)],format="%d,%m,%Y"),format="%m/%d/%Y")

#pd$seedlings75 <- as.character(pd$seedlings75)
pd$seedlings75[grep(",",pd$seedlings75)] <- format.Date(as.Date(pd$seedlings75[grep(",",pd$seedlings75)],format="%d,%m,%Y"),format="%m/%d/%Y")

#pd$flowering10 <- as.character(pd$flowering10)
pd$flowering10[grep(",",pd$flowering10)] <- format.Date(as.Date(pd$flowering10[grep(",",pd$flowering10)],format="%d,%m,%Y"),format="%m/%d/%Y")

#pd$flowering75 <- as.character(pd$flowering75)
pd$flowering75[grep(",",pd$flowering75)] <- format.Date(as.Date(pd$flowering75[grep(",",pd$flowering75)],format="%d,%m,%Y"),format="%m/%d/%Y")

#pd$maturityStart <- as.character(pd$maturityStart)
pd$maturityStart[grep(",",pd$maturityStart)] <- format.Date(as.Date(pd$maturityStart[grep(",",pd$maturityStart)],format="%d,%m,%Y"),format="%m/%d/%Y")

#pd$maturityFull <- as.character(pd$maturityFull)
pd$maturityFull[grep(",",pd$maturityFull)] <- format.Date(as.Date(pd$maturityFull[grep(",",pd$maturityFull)],format="%d,%m,%Y"),format="%m/%d/%Y")

# 1 - белая; 2 - светло-розовая; 3- розовая; 4 - сиренево-розовая; 5 - фиолетово-розовая; 6 - красно-фиолетовая; 7 - голубая; 8  - желто - зелёная

#pd$flowerColour <- as.character(pd$flowerColour)

pd$flowerColour[pd$flowerColour == " purple"] <- 6
pd$flowerColour[pd$flowerColour == "белый"] <- 1
pd$flowerColour[pd$flowerColour == "розовый"] <- 3
pd$flowerColour[pd$flowerColour == "ligt pink"] <- 2
pd$flowerColour[pd$flowerColour == "pink"] <- 3
pd$flowerColour[pd$flowerColour == "white"] <- 1

# 1 - стелющаяся; 3 - развалистая; 5 - стоячая (раскидистая вверху); 7 - стоячая (комактная)

#pd$bushShape <- as.character(pd$bushShape)

#pd1 <- pd[grepl(" ", pd$bushShape) & !grepl(" $", pd$bushShape),]
pd2 <- pd[grepl(" ", pd$bushShape) & !grepl(" $", pd$bushShape),]

pd$bushShape <- gsub(" .$", "", pd$bushShape)
pd2$bushShape <- gsub("^. ", "", pd2$bushShape)

pd$Ptht_1 <- gsub("/", "", gsub(" .*$", "", pd$Ptht_1))
pd2$Ptht_1 <- gsub("/", "", gsub("^.* ", "", pd2$Ptht_1))

pd$Hlp_1 <- gsub("/", "", gsub(" .*$", "", pd$Hlp_1))
pd2$Hlp_1 <- gsub("/", "", gsub("^.* ", "", pd2$Hlp_1))

pd$PPP_1 <- gsub("/", "", gsub(" .*$", "", pd$PPP_1))
pd2$PPP_1 <- gsub("/", "", gsub("^.* ", "", pd2$PPP_1))

pd$PDW_1 <- gsub("/", "", gsub(" .*$", "", pd$PDW_1))
pd2$PDW_1 <- gsub("/", "", gsub("^.* ", "", pd2$PDW_1))

pd$PDL_1 <- gsub("/", "", gsub(" .*$", "", pd$PDL_1))
pd2$PDL_1 <- gsub("/", "", gsub("^.* ", "", pd2$PDL_1))

pd$SPD_1 <- gsub("/", "", gsub(" .*$", "", pd$SPD_1))
pd2$SPD_1 <- gsub("/", "", gsub("^.* ", "", pd2$SPD_1))

pd$SCO <- gsub("/.*$", "", pd$SCO)
pd2$SCO <- gsub("^.*/", "", pd2$SCO)

pd$TSW <- gsub("/.*$", "", pd$TSW)
pd2$TSW <- gsub("^.*/", "", pd2$TSW)

pd$SYDP_1 <- gsub("/.*$", "", pd$SYDP_1)
pd2$SYDP_1 <- gsub("^.*/", "", pd2$SYDP_1)

pd <- rbind(pd, pd2)

# 1 - белая; 2 - желто-розовая; 3- розовая; 4 - желтая; 5 - серая; 6 - темно-зеленая; 7 - светло-зеленая; 8  - оранжевая; 9 - рыжая; 10 - коричневая; 11 - светло-коричневая; 12 - красно-коричневая; 13 красно-фиолетовая; 14 - черная

# бежевый желтый beige black brown dark brown grey yellow yelow

pd$SCO[pd$SCO == "бежевый"] <- 11
pd$SCO[pd$SCO == "желтый"] <- 4
pd$SCO[pd$SCO == "beige"] <- 11
pd$SCO[pd$SCO == "black"] <- 14
pd$SCO[pd$SCO == "brown"] <- 10
pd$SCO[pd$SCO == "dark brown"] <-  12
pd$SCO[pd$SCO == "grey"] <- 5
pd$SCO[pd$SCO == "yelow"] <- 4
pd$SCO[pd$SCO == "yellow"] <- 4

pd$PDW_1 <- gsub(",", ".", pd$PDW_1)
pd$PDL_1 <- gsub(",", ".", pd$PDL_1)

pd$SYDP_1 <- gsub(",", ".", pd$SYDP_1)
pd$SYDP_1[pd$SYDP_1 == "нет семян"] <- 0
pd$SYDP_1[pd$SYDP_1 == "единич.зерна"] <- 0

pd$TSW <- gsub(",", ".", pd$TSW)
pd$TSW[pd$TSW == "single grains"] <- 0

pd$PDH[pd$PDH == "no dehiscence"] <- 0
pd$PDH[pd$PDH == "lowe dehiscence"] <- 1
pd$PDH[pd$PDH == "dehiscence"] <- 2
pd$PDH[pd$PDH != 1 & pd$PDH != 2] <- 0

ii <- !duplicated(pd[, 5])

NN <- sum(ii)

vdf <- data.frame(cbind(c(pd[ii, 5], "ICCV96029"), c(rep(0, NN), 1), c(pd[ii, 7], "USA"), c(rep(1900, NN), 2017)))
colnames(vdf) <- c('name', 'catnumber',  'origin', 'colyear')
SQL <- sqlAppendTable(conn, "variety", vdf, row.names = FALSE)
dbSendStatement(conn, SQL)

envdf <- data.frame(envname="KOS_2017",
		location = "KOS",
		year = "2017")

SQL <- sqlAppendTable(conn, "environment", envdf, row.names = FALSE)
dbSendStatement(conn, SQL)
#odb.insert(ODB, "environment", envdf, execute = TRUE, dateFormat = "%m/%d/%Y")

gname <- paste0("ICCV96029_x_", pd[ii, 5])
gdf <- data.frame(cbind(gname, rep('ICCV96029', length(gname)), pd[ii, 5]))
colnames(gdf) <- c('gname', 'ancleft', 'ancright')
SQL <- sqlAppendTable(conn, "genotype", gdf, row.names = FALSE)
dbSendStatement(conn, SQL)

#odb.insert(ODB, "genotype", cbind(gname, rep('ICCV96029', length(gname)), pd[ii, 5]), execute = TRUE, dateFormat = "%m/%d/%Y")

#pd[20,]$maturityFull <- "03/08/2017"
#pd[21,]$maturityFull <- "03/08/2017"
#pd[22,]$maturityFull <- "03/08/2017"
#pd[39,]$maturityFull <- "06/08/2017"
#pd$flowering75[which(is.na(pd$flowering75))] <- pd$flowering10[which(is.na(pd$flowering75))]
#pd$flowering75[pd$flowering75 == ""] <- pd$flowering10[pd$flowering75 == ""]

apply(pd[-c(which(pd$flowering75 == ""), which(is.na(pd$flowering75)), which(is.na(pd$inseriesnum)), which(pd$flowering75 == "погиб")),], 1, FUN=function(r) {
	gt <- paste0("'ICCV96029_x_", r[5], "'")
	dat <- paste(gt, r[6],
		format.Date(as.Date(r[8], format = "%m/%d/%Y"), format="'%Y-%m-%d'"),
		format.Date(as.Date(r[11], format = "%m/%d/%Y"), format="'%Y-%m-%d'"),
		format.Date(as.Date(r[12], format = "%m/%d/%Y"), format="'%Y-%m-%d'"),
		format.Date(as.Date(r[13], format = "%m/%d/%Y"), format="'%Y-%m-%d'"),
		format.Date(as.Date(r[14], format = "%m/%d/%Y"), format="'%Y-%m-%d'"),
		format.Date(as.Date(r[15], format = "%m/%d/%Y"), format="'%Y-%m-%d'"),
		format.Date(as.Date(r[16], format = "%m/%d/%Y"), format="'%Y-%m-%d'"),
		r[25], r[26], r[27], r[28], r[29], r[30], r[31], r[32], r[33], r[34], r[35], r[36], r[38], "'KOS_2017'", sep = ',')
	qu <- paste0("INSERT INTO accession (genotype, inseriesnum, sowing, seedlings10, seedlings75, flowering10, flowering75, maturityStart, maturityFull, flowerColor, bushShape, Ptht, Hlp, PPP, PDW, PDL, SPD, SCO, TSW, SYDP, PDH, PodPed, env) VALUES (", dat, ")")
	cat(qu, '\n')
#	odb.write(ODB, qu)
	dbSendStatement(conn, qu)
})

apply(pd[which(pd$flowering75 == "погиб"),], 1, FUN=function(r) {
	gt <- paste0("'ICCV96029_x_", r[5], "'")
	dat <- paste(gt, r[6],
		format.Date(as.Date(r[8], format = "%m/%d/%Y"), format="'%Y-%m-%d'"),
		format.Date(as.Date(r[11], format = "%m/%d/%Y"), format="'%Y-%m-%d'"),
		format.Date(as.Date(r[12], format = "%m/%d/%Y"), format="'%Y-%m-%d'"),
		format.Date(as.Date(r[13], format = "%m/%d/%Y"), format="'%Y-%m-%d'"),
		r[36], r[38], "'KOS_2017'", sep = ',')
	qu <- paste0("INSERT INTO accession (genotype, inseriesnum, sowing, seedlings10, seedlings75, flowering10, PDH, PodPed, env) VALUES (", dat, ")")
	cat(qu, '\n')
#	odb.write(ODB, qu)
	dbSendStatement(conn, qu)
})

apply(pd[c(which(pd$flowering75 == ""), which(is.na(pd$flowering75))),], 1, FUN=function(r) {
	gt <- paste0("'ICCV96029_x_", r[5], "'")
	dat <- paste(gt, r[6],
		format.Date(as.Date(r[8], format = "%m/%d/%Y"), format="'%Y-%m-%d'"),
		format.Date(as.Date(r[11], format = "%m/%d/%Y"), format="'%Y-%m-%d'"),
		format.Date(as.Date(r[12], format = "%m/%d/%Y"), format="'%Y-%m-%d'"),
		format.Date(as.Date(r[13], format = "%m/%d/%Y"), format="'%Y-%m-%d'"),
		format.Date(as.Date(r[15], format = "%m/%d/%Y"), format="'%Y-%m-%d'"),
		format.Date(as.Date(r[16], format = "%m/%d/%Y"), format="'%Y-%m-%d'"),
		r[25], r[26], r[27], r[28], r[29], r[30], r[31], r[32], r[33], r[34], r[35], r[36], r[38], "'KOS_2017'", sep = ',')
	qu <- paste0("INSERT INTO accession (genotype, inseriesnum, sowing, seedlings10, seedlings75, flowering10, maturityStart, maturityFull, flowerColor, bushShape, Ptht, Hlp, PPP, PDW, PDL, SPD, SCO, TSW, SYDP, PDH, PodPed, env) VALUES (", dat, ")")
	cat(qu, '\n')
#	odb.write(ODB, qu)
	dbSendStatement(conn, qu)
})

kgl <- read.csv('../data-kruglic.csv', comment.char="#", header = TRUE)

NL <- sum(nlevels(kgl$DD),
		nlevels(kgl$N),
		nlevels(kgl$WW),
		nlevels(kgl$W1),
		nlevels(kgl$W2),
		nlevels(kgl$Cl),
		nlevels(kgl$Nh),
		nlevels(kgl$H),
		nlevels(kgl$Cm),
		nlevels(kgl$Ch),
		nlevels(kgl$E),
		nlevels(kgl$E.))

rp5lev <- data.frame(id = 1:NL, name = c(rep('DD', nlevels(kgl$DD)),
	rep('N', nlevels(kgl$N)),
	rep('WW', nlevels(kgl$WW)),
	rep('W1', nlevels(kgl$W1)),
	rep('W2', nlevels(kgl$W2)),
	rep('Cl', nlevels(kgl$Cl)),
	rep('Nh', nlevels(kgl$Nh)),
	rep('H', nlevels(kgl$H)),
	rep('Cm', nlevels(kgl$Cm)),
	rep('Ch', nlevels(kgl$Ch)),
	rep('E', nlevels(kgl$E)),
	rep('E1', nlevels(kgl$E.))
	),
	explanation = c(levels(kgl$DD),
		levels(kgl$N),
		levels(kgl$WW),
		levels(kgl$W1),
		levels(kgl$W2),
		levels(kgl$Cl),
		levels(kgl$Nh),
		levels(kgl$H),
		levels(kgl$Cm),
		levels(kgl$Ch),
		levels(kgl$E),
		levels(kgl$E.)),
	level = c(seq(1:nlevels(kgl$DD)),
		seq(1:nlevels(kgl$N)),
		seq(1:nlevels(kgl$WW)),
		seq(1:nlevels(kgl$W1)),
		seq(1:nlevels(kgl$W2)),
		seq(1:nlevels(kgl$Cl)),
		seq(1:nlevels(kgl$Nh)),
		seq(1:nlevels(kgl$H)),
		seq(1:nlevels(kgl$Cm)),
		seq(1:nlevels(kgl$Ch)),
		seq(1:nlevels(kgl$E)),
		seq(1:nlevels(kgl$E.))
	)
)

SQL <- sqlAppendTable(conn, "rp5levels", rp5lev, row.names = FALSE)
dbSendStatement(conn, SQL)
#odb.insert(ODB, "rp5levels", rp5lev, execute = TRUE, dateFormat = "%m/%d/%Y")

kgl$DD <- as.numeric(kgl$DD)
kgl$N <- as.numeric(kgl$N)
kgl$WW <- as.numeric(kgl$WW)
kgl$W1 <- as.numeric(kgl$W1)
kgl$W2 <- as.numeric(kgl$W2)
kgl$Cl <- as.numeric(kgl$Cl)
kgl$Nh <- as.numeric(kgl$Nh)
kgl$H <- as.numeric(kgl$H)
kgl$Cm <- as.numeric(kgl$Cm)
kgl$Ch <- as.numeric(kgl$Ch)
kgl$E <- as.numeric(kgl$E)
kgl$E. <- as.numeric(kgl$E.)

kgl[,1] <- as.character(kgl[,1])
kgl[,1] <- paste("'",strptime(kgl[,1], format = "%d.%m.%Y%t%R"), "'", sep = "")

kgl$sss <- as.character(kgl$sss)
kgl$sss[kgl$sss == ""] <- 0
kgl$sss[kgl$sss == "Менее 0.5"] <- 0
kgl$sss <- as.numeric(kgl$sss)

kgl[,1] <- gsub("'", "", kgl[,1])

kgl$RRR <- as.character(kgl$RRR)
kgl$RRR[kgl$RRR == ""] <- 0
kgl$RRR[kgl$RRR == "Осадков нет"] <- 0
kgl$RRR[kgl$RRR == "Следы осадков"] <- 0.0000000000000001
kgl$RRR <- as.numeric(kgl$RRR)

kdf <- data.frame(cbind(1:nrow(kgl), rep('KOS', nrow(kgl)), kgl))
colnames(kdf) <- c("id", "location", "tsp", tolower(gsub(".", "1", colnames(kgl)[-1], fixed = TRUE)))
SQL <- sqlAppendTable(conn, "rp5data", kdf, row.names = FALSE)
dbSendStatement(conn, SQL)

#odb.insert(ODB, "rp5data", cbind(1:nrow(kgl), rep('KOS', nrow(kgl)), kgl), execute = TRUE)

library(vcfR)
ivcf <- read.vcfR('/storage3/kkozlov/Calcs/chickpea-/ICCV96029_Ce.CDCFrontier_GA_v1.VF.SNP.vcf.gz')
snpdf <- as.data.frame(slot(ivcf, 'fix'))
snpabbr <- paste(snpdf$CHROM, snpdf$POS, snpdf$REF, snpdf$ALT, sep = "_")
Al <- sapply(as.character(snpdf$ALT), FUN = function(r) {
		a <- grep("A", unlist(strsplit(r, ",")), fixed = TRUE)
		if (!is.na(a[1])) {
			return(a[1])
		} else {
			return(0)
		}

	})

Cl <- sapply(as.character(snpdf$ALT), FUN = function(r) {
		a <- grep("C", unlist(strsplit(r, ",")), fixed = TRUE)
		if (!is.na(a[1])) {
			return(a[1])
		} else {
			return(0)
		}

	})

Gl <- sapply(as.character(snpdf$ALT), FUN = function(r) {
		a <- grep("G", unlist(strsplit(r, ",")), fixed = TRUE)
		if (!is.na(a[1])) {
			return(a[1])
		} else {
			return(0)
		}

	})

Tl <- sapply(as.character(snpdf$ALT), FUN = function(r) {
		a <- grep("T", unlist(strsplit(r, ",")), fixed = TRUE)
		if (!is.na(a[1])) {
			return(a[1])
		} else {
			return(0)
		}

	})

head(cbind(snpabbr, as.character(snpdf$CHROM), as.character(snpdf$POS), as.character(snpdf$REF), Al, Cl, Gl, Tl, as.character(snpdf$QUAL)))

cfdf <- data.frame(cbind(snpabbr, as.character(snpdf$CHROM), as.character(snpdf$POS), as.character(snpdf$REF), Al, Cl, Gl, Tl, as.character(snpdf$QUAL)))
colnames(cfdf) <- c('snpabbr', 'chrname', 'pos', 'ref', 'a', 'c', 'g', 't', 'quality')
SQL <- sqlAppendTable(conn, "snp", cfdf, row.names = FALSE)
dbSendStatement(conn, SQL)

#odb.insert(ODB, "snp", cbind(snpabbr, as.character(snpdf$CHROM), as.character(snpdf$POS), as.character(snpdf$REF), Al, Cl, Gl, Tl, as.character(snpdf$QUAL)), execute = TRUE)

gtsnpdf <- as.data.frame(slot(ivcf, 'gt'))

#head(gtsnpdf)

mgt <- dim(gtsnpdf)[2]
ngt <- colnames(gtsnpdf)
msnp <- dim(gtsnpdf)[1]

qg <- lapply(2:mgt, FUN = function(cc) {
	ancs = unlist(strsplit(ngt[cc], "_"))
	tryCatch(
		{
			cat(paste0("INSERT INTO variety (name, catnumber, origin, colyear) values ('", ancs[1], "',", 0, ',', "'tab'", ',', 1900, ")"), '\n')
			dbSendStatement(conn, paste0("INSERT INTO variety (name, catnumber, origin, colyear) values ('", ancs[1], "',", 0, ',', "'tab'", ',', 1900, ")"))
		},
        error=function(cond) {
            message(cond)
            # Choose a return value in case of error
            return(NA)
        },
        warning=function(cond) {
            message(cond)
            # Choose a return value in case of warning
            return(NULL)
        },
        finally={
        # NOTE:
        # Here goes everything that should be executed at the end,
        # regardless of success or error.
        # If you want more than one expression to be executed, then you
        # need to wrap them in curly brackets ({...}); otherwise you could
        # just have written 'finally=<expression>'
            message("Some other message at the end")
        }
    )
	tryCatch(
		{
			cat(paste0("INSERT INTO variety (name, catnumber, origin, colyear) values ('", paste(ancs[c(-1, -2)], sep = "_", collapse = "_"), "',", 0, ',', "tab", ',', 1900, ")"), '\n')
			dbSendStatement(conn, paste0("INSERT INTO variety (name, catnumber, origin, colyear) values ('", paste(ancs[c(-1, -2)], sep = "_", collapse = "_"), "',", 0, ',', "'tab'", ',', 1900, ")"))
		},
        error=function(cond) {
            message(cond)
            # Choose a return value in case of error
            return(NA)
        },
        warning=function(cond) {
            message(cond)
            # Choose a return value in case of warning
            return(NULL)
        },
        finally={
        # NOTE:
        # Here goes everything that should be executed at the end,
        # regardless of success or error.
        # If you want more than one expression to be executed, then you
        # need to wrap them in curly brackets ({...}); otherwise you could
        # just have written 'finally=<expression>'
            message("Some other message at the end")
        }
    )
	tryCatch(
		{
			dbSendStatement(conn, paste0("INSERT INTO genotype (gname, ancleft, ancright) values ('", ngt[cc], "','", ancs[1], "','", paste(ancs[c(-1, -2)], sep = "_", collapse = "_"), "')"))
		},
        error=function(cond) {
            message(cond)
            # Choose a return value in case of error
            return(NA)
        },
        warning=function(cond) {
            message(cond)
            # Choose a return value in case of warning
            return(NULL)
        },
        finally={
        # NOTE:
        # Here goes everything that should be executed at the end,
        # regardless of success or error.
        # If you want more than one expression to be executed, then you
        # need to wrap them in curly brackets ({...}); otherwise you could
        # just have written 'finally=<expression>'
            message("Some other message at the end")
        }
    )
	qd <- lapply(1:msnp, FUN = function(rr) {
			datum = as.character(gtsnpdf[rr, cc])
			if (datum == "./.:.:.:.:.") {
				return(NA)
			}
#			cat(datum, '==' )
			codes = unlist(strsplit(datum, ":"))
			ndom = sum((as.numeric(unlist(strsplit(codes[1], "/"))) == 0))
			alt = unlist(strsplit(codes[2], ","))
			REF = alt[1]
			ALT1 = 0
			ALT2 = 0
			ALT3 = 0
			if (length(alt) > 1) {
				ALT1 = alt[2]
				if (length(alt) > 2) {
					ALT2 = alt[3]
					if (length(alt) > 3) {
						ALT2 = alt[4]
					}
				}
			}
#			cat(ndom, REF, ALT1, ALT2, ALT3, '\n')
			qu <- paste0("INSERT INTO gt_snp (gt, snpabbr, ndom, ref, alt1, alt2, alt3) VALUES (", "'", ngt[cc], "'", ",", "'", snpabbr[rr], "'", ",", ndom, ",", REF, ",", ALT1, ",", ALT2, ",", ALT3, ")")
			cat(qu, '\n')
			dbSendStatement(conn, qu)
		})
	})


#
# for y in /storage3/asokolkova/finding_CNV/cnv_files/*after_filtration.txt;do mkdir `basename $y | sed -e "s/_Ca1.*//g"`;ln -s $y `basename $y | sed -e "s/_Ca1.*//g"`;done
#
library(intansv)
cnvoutputdir <- "cnvr"
availdat <- list.dirs(path = cnvoutputdir, full.names = FALSE, recursive = FALSE)
ret <- lapply(availdat, FUN = function(a) {
	cat(a, '\n')
	tryCatch(
	    {
		dbSendStatement(conn, paste0("INSERT INTO variety (name, catnumber, origin, colyear) values ('", a, "',", 0, ',', "'ctab'", ',', 1900, ")"))
	    },
	    error = function(cond) { message(cond); return(NA) },
	    warning = function(cond) { message(cond); return(NULL)},
	    finally = { message("Some other message at the end") }
	)
	tryCatch(
	    {
		dbSendStatement(conn, paste0("INSERT INTO genotype (gname, ancleft, ancright) values ('", a, "','", a, "','", a, "')"))
	    },
	    error = function(cond) { message(cond); return(NA) },
	    warning = function(cond) { message(cond); return(NULL)},
	    finally = { message("Some other message at the end") }
	)
})

ret <- lapply(availdat, FUN = function(a) {
	    cnvr_a <- tryCatch(
		{
		    cnvr_a <- readCnvnator(dataDir = file.path(cnvoutputdir, a), regSizeLowerCutoff = 100, regSizeUpperCutoff = 1000000, method="CNVnator")
		},
	    error=function(cond) { message(cond); return(NULL)},
	    warning=function(cond) { message(cond); return(NULL) },
	    finally={ message("Some other message at the end") })
	    if (!is.null(cnvr_a)) {
			ret_a <- apply(cnvr_a$dup, 1, FUN = function(u) {
				cnpabbr <- paste(u[1], as.numeric(u[2]), as.numeric(u[3]), sep = "_")
				w = tryCatch(
				    {
					qu <- paste0("INSERT INTO cnum (cnabbr, chrname, coordstart, coordend, length) values ('", cnpabbr, "','", u[1], "',", as.numeric(u[2]), ",", as.numeric(u[3]),",", as.numeric(u[4]), ")", '\n')
					cat(qu, '\n')
					dbSendStatement(conn, qu)
				    },
				    error = function(cond) { message(cond); return(NA) },
				    warning = function(cond) { message(cond); return(NULL)},
				    finally = { message("Some other message at the end") }
				)
			})
			cat(' GOOD \n')
	    }
})

ret <- lapply(availdat, FUN = function(a) {
	    cnvr_a <- tryCatch(
		{
		    cnvr_a <- readCnvnator(dataDir = file.path(cnvoutputdir, a), regSizeLowerCutoff = 100, regSizeUpperCutoff = 1000000, method="CNVnator")
		},
	    error=function(cond) { message(cond); return(NULL)},
	    warning=function(cond) { message(cond); return(NULL) },
	    finally={ message("Some other message at the end") })
	    if (!is.null(cnvr_a)) {
			ret_a <- apply(cnvr_a$del, 1, FUN = function(u) {
				cnpabbr <- paste(u[1], as.numeric(u[2]), as.numeric(u[3]), sep = "_")
				tryCatch(
				    {
					qu <- paste0("INSERT INTO cnum (cnabbr, chrname, coordstart, coordend, length) values ('", cnpabbr, "','", u[1], "',", as.numeric(u[2]), ",", as.numeric(u[3]),",", as.numeric(u[4]), ")", '\n')
					cat(qu, '\n')
					dbSendStatement(conn, qu)
				    },
				    error = function(cond) { message(cond); return(NA) },
				    warning = function(cond) { message(cond); return(NULL)},
				    finally = { message("Some other message at the end") }
				)
			})
		#    }
			cat(' GOOD \n')
	    }
})

ret <- lapply(availdat, FUN = function(a) {
	    cnvr_a <- tryCatch(
		{
		    cnvr_a <- readCnvnator(dataDir = file.path(cnvoutputdir, a), regSizeLowerCutoff = 100, regSizeUpperCutoff = 1000000, method="CNVnator")
		},
	    error=function(cond) { message(cond); return(NULL)},
	    warning=function(cond) { message(cond); return(NULL) },
	    finally={ message("Some other message at the end") })
	    if (!is.null(cnvr_a)) {
			ret_a <- apply(cnvr_a$inv, 1, FUN = function(u) {
				cnpabbr <- paste(u[1], as.numeric(u[2]), as.numeric(u[3]), sep = "_")
				tryCatch(
				    {
					qu <- paste0("INSERT INTO cnum (cnabbr, chrname, coordstart, coordend, length) values ('", cnpabbr, "','", u[1], "',", as.numeric(u[2]), ",", as.numeric(u[3]),",", as.numeric(u[4]), ")", '\n')
					cat(qu, '\n')
					dbSendStatement(conn, qu)
				    },
				    error = function(cond) { message(cond); return(NA) },
				    warning = function(cond) { message(cond); return(NULL)},
				    finally = { message("Some other message at the end") }
				)
			})
		#    }
			cat(' GOOD \n')
	    }
})

ret <- lapply(availdat, FUN = function(a) {
	    cnvr_a <- tryCatch(
		{
		    cnvr_a <- readCnvnator(dataDir = file.path(cnvoutputdir, a), regSizeLowerCutoff = 100, regSizeUpperCutoff = 1000000, method="CNVnator")
		},
	    error=function(cond) { message(cond); return(NULL)},
	    warning=function(cond) { message(cond); return(NULL) },
	    finally={ message("Some other message at the end") })
	    if (!is.null(cnvr_a)) {
			ret_a <- apply(cnvr_a$dup, 1, FUN = function(u) {
				cnpabbr <- paste(u[1], as.numeric(u[2]), as.numeric(u[3]), sep = "_")
				qu <- paste0("INSERT INTO gt_cnum (gt, cnabbr, cnumber) VALUES (", "'", a, "'", ",", "'", cnpabbr, "'", ",", 2, ")")
				cat(qu, '\n')
				tryCatch( { dbSendStatement(conn, qu) },
				    error = function(cond) { message(cond); return(NA) },
				    warning = function(cond) { message(cond); return(NULL)},
				    finally = { message("Some other message at the end") }
				)
			})
			ret_a <- apply(cnvr_a$del, 1, FUN = function(u) {
				cnpabbr <- paste(u[1], as.numeric(u[2]), as.numeric(u[3]), sep = "_")
				qu <- paste0("INSERT INTO gt_cnum (gt, cnabbr, cnumber) VALUES (", "'", a, "'", ",", "'", cnpabbr, "'", ",", 0, ")")
				cat(qu, '\n')
				tryCatch( { dbSendStatement(conn, qu) },
				    error = function(cond) { message(cond); return(NA) },
				    warning = function(cond) { message(cond); return(NULL)},
				    finally = { message("Some other message at the end") }
				)
			})
#			print(dim(cnvr_a$inv))
#			ret_a <- apply(cnvr_a$inv, 1, FUN = function(u) {
#				cnpabbr <- paste(u[1], as.numeric(u[2]), as.numeric(u[3]), sep = "_")
#				qu <- paste0("INSERT INTO gt_cnum (gt, cnabbr, cnumber) VALUES (", "'", a, "'", ",", "'", cnpabbr, "'", ",", 1, ")")
#				cat(qu, '\n')
#				tryCatch( { dbSendStatement(conn, qu) },
#				    error = function(cond) { message(cond); return(NA) },
#				    warning = function(cond) { message(cond); return(NULL)},
#				    finally = { message("Some other message at the end") }
#				)
#			})
		#    }
			cat(' GOOD \n')
	    }
})
