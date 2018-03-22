corrds <- data.frame(
    locname = c("KOS", "Floreat", "Mt. Barker", "Ankara", "Urfa"),
    lat = c(45.216313, -31.935988, -34.629784, 39.920777, 37.215282),
    lon = c(40.797045, 115.780192, 117.666196, 32.854058, 38.737966)
)

locdf <- data.frame(locname = "KOS",
		country = "Russia",
		region = "Kuban",
		lat = corrds[corrds$locname == "KOS", 2],
		lon = corrds[corrds$locname == "KOS", 3])

SQL <- sqlAppendTable(conn, "location", locdf, row.names = FALSE)
dbSendStatement(conn, SQL)

kos2016meteo <- read.csv('./meteo2016.csv')
mdf <- data.frame(cbind(1:5, rep('KOS', 5), rep(2016, 5), 4:8, t(kos2016meteo[,-1])))
colnames(mdf) <- c('id', 'location', 'year', 'month', 'tmean', 'pmean', 'pdays', 'tsum10', 'tsoil', 'hrel')
SQL <- sqlAppendTable(conn, "meteo", mdf, row.names = FALSE)
dbSendStatement(conn, SQL)

kos2017meteo <- read.csv('./meteo2017.csv')
mdf <- data.frame(cbind(6:10, rep('KOS', 5), rep(2017, 5), 4:8, t(kos2017meteo[,-1])))
colnames(mdf) <- c('id', 'location', 'year', 'month', 'tmean', 'pmean', 'pdays', 'tsum10', 'tsoil', 'hrel')
SQL <- sqlAppendTable(conn, "meteo", mdf, row.names = FALSE)
dbSendStatement(conn, SQL)

pd <- read.csv('../chickpea-doug/pheno_doug_2017.csv', header = 1, stringsAsFactors = FALSE)

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
colnames(vdf) <- c('name', 'catnumber',  'origin_country', 'colyear')
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

vdf <- data.frame(cbind(gname, rep(0, length(gname)), rep('loc', length(gname)), rep(1900, length(gname)), gname))
colnames(vdf) <- c('name', 'catnumber',  'origin_country', 'colyear', 'gtname')
SQL <- sqlAppendTable(conn, "variety", vdf, row.names = FALSE)
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
	qu <- paste0("INSERT INTO accession (variety, inseriesnum, sowing, seedlings10, seedlings75, flowering10, flowering75, maturityStart, maturityFull, flowerColor, bushShape, Ptht, Hlp, PPP, PDW, PDL, SPD, SCO, TSW, SYDP, PDH, PodPed, env) VALUES (", dat, ")")
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
	qu <- paste0("INSERT INTO accession (variety, inseriesnum, sowing, seedlings10, seedlings75, flowering10, PDH, PodPed, env) VALUES (", dat, ")")
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
	qu <- paste0("INSERT INTO accession (variety, inseriesnum, sowing, seedlings10, seedlings75, flowering10, maturityStart, maturityFull, flowerColor, bushShape, Ptht, Hlp, PPP, PDW, PDL, SPD, SCO, TSW, SYDP, PDH, PodPed, env) VALUES (", dat, ")")
	cat(qu, '\n')
#	odb.write(ODB, qu)
	dbSendStatement(conn, qu)
})

kgl <- read.csv('../chickpea-doug/data-kruglic.csv', comment.char="#", header = TRUE)

#NL <- sum(nlevels(kgl$DD),
#		nlevels(kgl$N),
#		nlevels(kgl$WW),
#		nlevels(kgl$W1),
#		nlevels(kgl$W2),
#		nlevels(kgl$Cl),
#		nlevels(kgl$Nh),
#		nlevels(kgl$H),
#		nlevels(kgl$Cm),
#		nlevels(kgl$Ch),
#		nlevels(kgl$E),
#		nlevels(kgl$E.))
#
#SQL <- sqlAppendTable(conn, "rp5levels", rp5lev, row.names = FALSE)
#dbSendStatement(conn, SQL)

SQL <- sqlAppendTable(conn, "rp5levels_dd", data.frame(level = c(seq(1:nlevels(kgl$DD))), explanation = c(levels(kgl$DD))), row.names = FALSE)
dbSendStatement(conn, SQL)
SQL <- sqlAppendTable(conn, "rp5levels_n", data.frame(level = c(seq(1:nlevels(kgl$N))), explanation = c(levels(kgl$N))), row.names = FALSE)
dbSendStatement(conn, SQL)
SQL <- sqlAppendTable(conn, "rp5levels_ww", data.frame(level = c(seq(1:nlevels(kgl$WW))), explanation = c(levels(kgl$WW))), row.names = FALSE)
dbSendStatement(conn, SQL)
SQL <- sqlAppendTable(conn, "rp5levels_w1", data.frame(level = c(seq(1:nlevels(kgl$W1))), explanation = c(levels(kgl$W1))), row.names = FALSE)
dbSendStatement(conn, SQL)
SQL <- sqlAppendTable(conn, "rp5levels_w2", data.frame(level = c(seq(1:nlevels(kgl$W2))), explanation = c(levels(kgl$W2))), row.names = FALSE)
dbSendStatement(conn, SQL)
SQL <- sqlAppendTable(conn, "rp5levels_cl", data.frame(level = c(seq(1:nlevels(kgl$Cl))), explanation = c(levels(kgl$Cl))), row.names = FALSE)
dbSendStatement(conn, SQL)
SQL <- sqlAppendTable(conn, "rp5levels_nh", data.frame(level = c(seq(1:nlevels(kgl$Nh))), explanation = c(levels(kgl$Nh))), row.names = FALSE)
dbSendStatement(conn, SQL)
SQL <- sqlAppendTable(conn, "rp5levels_h", data.frame(level = c(seq(1:nlevels(kgl$H))), explanation = c(levels(kgl$H))), row.names = FALSE)
dbSendStatement(conn, SQL)
SQL <- sqlAppendTable(conn, "rp5levels_cm", data.frame(level = c(seq(1:nlevels(kgl$Cm))), explanation = c(levels(kgl$Cm))), row.names = FALSE)
dbSendStatement(conn, SQL)
SQL <- sqlAppendTable(conn, "rp5levels_ch", data.frame(level = c(seq(1:nlevels(kgl$Ch))), explanation = c(levels(kgl$Ch))), row.names = FALSE)
dbSendStatement(conn, SQL)
SQL <- sqlAppendTable(conn, "rp5levels_e", data.frame(level = c(seq(1:nlevels(kgl$E))), explanation = c(levels(kgl$E))), row.names = FALSE)
dbSendStatement(conn, SQL)
SQL <- sqlAppendTable(conn, "rp5levels_e1", data.frame(level = c(seq(1:nlevels(kgl$E.))), explanation = c(levels(kgl$E.))), row.names = FALSE)
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

kdf <- data.frame(cbind(1:nrow(kgl), rep('KOS', nrow(kgl)), kgl, daylength(corrds[corrds$locname == "KOS", 2], kgl[, 1])))
colnames(kdf) <- c("id", "location", "tsp", tolower(gsub(".", "1", colnames(kgl)[-1], fixed = TRUE)), "dl")
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
			cat(paste0("INSERT INTO variety (name, catnumber, origin_country, colyear) values ('", ancs[1], "',", 0, ',', "'tab'", ',', 1900, ")"), '\n')
			dbSendStatement(conn, paste0("INSERT INTO variety (name, catnumber, origin_country, colyear) values ('", ancs[1], "',", 0, ',', "'tab'", ',', 1900, ")"))
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
			cat(paste0("INSERT INTO variety (name, catnumber, origin_country, colyear) values ('", paste(ancs[c(-1, -2)], sep = "_", collapse = "_"), "',", 0, ',', "tab", ',', 1900, ")"), '\n')
			dbSendStatement(conn, paste0("INSERT INTO variety (name, catnumber, origin_country, colyear) values ('", paste(ancs[c(-1, -2)], sep = "_", collapse = "_"), "',", 0, ',', "'tab'", ',', 1900, ")"))
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
		dbSendStatement(conn, paste0("INSERT INTO variety (name, catnumber, origin_country, colyear) values ('", a, "',", 0, ',', "'ctab'", ',', 1900, ")"))
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


pd <- read.csv('kos_2017.csv', header = 1, stringsAsFactors = FALSE)

pd <- pd[, -c(84, 85, 86)]

#"ancright","inseriesnum",
#,"Number.sowing.seeds","Number.of.rows"

colnames(pd) <- c("X","spot","catnumber","variety","origin","sowing","seedlings10","seedlings75","flowering10","flowering75", "floweringFin", "maturityStart","maturityFull","Sowing...the.beginning.seedlings.days.",
"Beginning.seedlings...the.beginning.of.flowering..days.",
"Beginning.seedlings...full.of.flowering..days.",
"Beginning.of.flowering...full.of.flowering..days.",
"Beginning.of.flowering...the.beginning.of.ripening..days.",
"Beginning.seedlings...the.beginning.of.ripening..days.",
"Beginning.seedlings...full.maturation..days.",
"The.beginning.of.ripening...full.maturation..days.",
"flowerColour",
"stemColor",
"bushShape",
"leafSize",
"peduncleColor",
"ascDamage",
"stemBranching",
"stemBranch1Length",
"stemBranch1BranchingType",
"stemBranch2BranchingType",
"PSH",
"PDH",
'Ptht.1','Ptht.2','Ptht.3','Ptht.4','Ptht.5','Hlp.1','Hlp.2','Hlp.3','Hlp.4','Hlp.5','Byld.1','Byld.2','Byld.3','Byld.4','Byld.5','WpWp.1','WpWp.2','WpWp.3','WpWp.4','WpWp.5','PPP.1','PPP.2','PPP.3','PPP.4','PPP.5','SPP.1','SPP.2','SPP.3','SPP.4','SPP.5','SYDS.1','SYDS.2','SYDS.3','SYDS.4','SYDS.5','PodSH','PDL.1','PDL.2','PDL.3','PDL.4','PDL.5','PDW.1','PDW.2','PDW.3','PDW.4','PDW.5','SSH','SCO','TSW','NsamplesColl')

# SPD_1 - number of seeds per pod
# SYDP_1 - weight.of.seeds.per.plot..g
# PodPed - Number.of.pod.per.Peduncle

#pd$seedlings10[grep(",",pd$seedlings10)] <- format.Date(as.Date(pd$seedlings10[grep(",",pd$seedlings10)],format="%d,%m,%Y"),format="%m/%d/%Y")

##pd$seedlings75 <- as.character(pd$seedlings75)
#pd$seedlings75[grep(",",pd$seedlings75)] <- format.Date(as.Date(pd$seedlings75[grep(",",pd$seedlings75)],format="%d,%m,%Y"),format="%m/%d/%Y")

##pd$flowering10 <- as.character(pd$flowering10)
#pd$flowering10[grep(",",pd$flowering10)] <- format.Date(as.Date(pd$flowering10[grep(",",pd$flowering10)],format="%d,%m,%Y"),format="%m/%d/%Y")

##pd$flowering75 <- as.character(pd$flowering75)
#pd$flowering75[grep(",",pd$flowering75)] <- format.Date(as.Date(pd$flowering75[grep(",",pd$flowering75)],format="%d,%m,%Y"),format="%m/%d/%Y")

##pd$maturityStart <- as.character(pd$maturityStart)
#pd$maturityStart[grep(",",pd$maturityStart)] <- format.Date(as.Date(pd$maturityStart[grep(",",pd$maturityStart)],format="%d,%m,%Y"),format="%m/%d/%Y")

##pd$maturityFull <- as.character(pd$maturityFull)
#pd$maturityFull[grep(",",pd$maturityFull)] <- format.Date(as.Date(pd$maturityFull[grep(",",pd$maturityFull)],format="%d,%m,%Y"),format="%m/%d/%Y")

# 1 - белая; 2 - светло-розовая; 3- розовая; 4 - сиренево-розовая; 5 - фиолетово-розовая; 6 - красно-фиолетовая; 7 - голубая; 8  - желто - зелёная

pd[pd == "нет"] <- NA
pd[pd == "Нет растений"] <- NA
pd[pd == "нет растений"] <- NA
pd[pd == "нет всходов"] <- NA
pd[pd == "Нет всходов"] <- NA
pd[pd == ""] <- NA

#unique(pd$flowerColour)
# "нет"          "Нет растений"
# "нет всходов"  "1.2"          "7"            "нет растений"

pd$flowerColour[pd$flowerColour == "1.2"] <- 2

unique(pd$"stemColor")
unique(pd$"bushShape")
unique(pd$"leafSize")
unique(pd$"peduncleColor")
unique(pd$"ascDamage")
unique(pd$"stemBranching")
unique(pd$"stemBranch1Length")
unique(pd$"stemBranch1BranchingType")
unique(pd$"stemBranch2BranchingType")
unique(pd$"PSH")
unique(pd$"PDH")
unique(pd$"flowering75")

ii <- !duplicated(pd[, 4])

NN <- sum(ii)

vdf <- data.frame(cbind(pd[ii, 4], pd[ii, 3], pd[ii, 5], rep(1900, NN)))
colnames(vdf) <- c('name', 'catnumber',  'origin_country', 'colyear')
SQL <- sqlAppendTable(conn, "variety", vdf, row.names = FALSE)
dbSendStatement(conn, SQL)

apply(pd, 1, FUN=function(r) {
    for(isn in 1:5) {
	gt <- paste0("'", r[4], "'")
	dat <- gsub("NA", "NULL", paste(isn,
		format.Date(as.Date(r[6], format = "%m/%d/%Y"), format="'%Y-%m-%d'"),
		format.Date(as.Date(r[7], format = "%m/%d/%Y"), format="'%Y-%m-%d'"),
		format.Date(as.Date(r[8], format = "%m/%d/%Y"), format="'%Y-%m-%d'"),
		format.Date(as.Date(r[9], format = "%m/%d/%Y"), format="'%Y-%m-%d'"),
		format.Date(as.Date(r[10], format = "%m/%d/%Y"), format="'%Y-%m-%d'"),
		format.Date(as.Date(r[11], format = "%m/%d/%Y"), format="'%Y-%m-%d'"),
		format.Date(as.Date(r[12], format = "%m/%d/%Y"), format="'%Y-%m-%d'"),
		format.Date(as.Date(r[13], format = "%m/%d/%Y"), format="'%Y-%m-%d'"),

		r[22], r[23], r[24], r[25], r[26], r[27], r[28], r[29], r[30], r[31], r[32], r[33],
		r[33 + isn], r[38 + isn], r[43 + isn], r[48 + isn], r[53 + isn], r[58 + isn], r[63 + isn], r[69], r[69 + isn], r[74 + isn],
		r[80], r[81], r[82],

		r[2], "'KOS_2017'", sep = ','))
	qu <- paste0("INSERT INTO accession (variety, inseriesnum, sowing, seedlings10, seedlings75, flowering10, flowering75, floweringFin, maturityStart, maturityFull, flowerColor,stemColor,bushShape,leafSize,peduncleColor,ascDamage,stemBranching, stemBranch1Length,stemBranch1BranchingType,stemBranch2BranchingType,PSH,PDH,
	Ptht, Hlp, Byld, WpWp, PPP, SPP, SYDS, PodSH, PDL, PDW,
	SSH, SCO, TSW, spot, env) VALUES (", gt, ',', dat, ")")
	cat(qu, '\n')
	dbSendStatement(conn, qu)
    }
})


pd <- read.csv('kos_2016.csv', header = 1, stringsAsFactors = FALSE)

#"ancright","inseriesnum",
#,"Number.sowing.seeds","Number.of.rows"

colnames(pd) <- c("X","spot","origin","catnumber","variety","sowing","seedlings10","seedlings75","flowering10","flowering75", "floweringFin", "maturityStart","maturityFull","Sowing...the.beginning.seedlings.days.",
"Beginning.seedlings...the.beginning.of.flowering..days.",
"Beginning.seedlings...full.of.flowering..days.",
"Beginning.of.flowering...full.of.flowering..days.",
"Beginning.of.flowering...the.beginning.of.ripening..days.",
"Beginning.seedlings...the.beginning.of.ripening..days.",
"Beginning.seedlings...full.maturation..days.",
"The.beginning.of.ripening...full.maturation..days.",
"flowerColour",
"stemColor",
"bushShape",
"leafSize",
"peduncleColor",
"ascDamage","ascResistance",
"stemBranching",
"stemBranch1Length",
"stemBranch1BranchingType",
"stemBranch2BranchingType",
"PSH",
"PDH",
'Ptht.1','Ptht.2','Ptht.3','Ptht.4','Ptht.5','Hlp.1','Hlp.2','Hlp.3','Hlp.4','Hlp.5','Byld.1','Byld.2','Byld.3','Byld.4','Byld.5','WpWp.1','WpWp.2','WpWp.3','WpWp.4','WpWp.5','PPP.1','PPP.2','PPP.3','PPP.4','PPP.5','SPP.1','SPP.2','SPP.3','SPP.4','SPP.5','SYDS.1','SYDS.2','SYDS.3','SYDS.4','SYDS.5','PodSH','PDL.1','PDL.2','PDL.3','PDL.4','PDL.5','PDW.1','PDW.2','PDW.3','PDW.4','PDW.5','SSH','SCO','TSW')

# SPD_1 - number of seeds per pod
# SYDP_1 - weight.of.seeds.per.plot..g
# PodPed - Number.of.pod.per.Peduncle

#pd$seedlings10[grep(",",pd$seedlings10)] <- format.Date(as.Date(pd$seedlings10[grep(",",pd$seedlings10)],format="%d,%m,%Y"),format="%m/%d/%Y")

##pd$seedlings75 <- as.character(pd$seedlings75)
#pd$seedlings75[grep(",",pd$seedlings75)] <- format.Date(as.Date(pd$seedlings75[grep(",",pd$seedlings75)],format="%d,%m,%Y"),format="%m/%d/%Y")

##pd$flowering10 <- as.character(pd$flowering10)
#pd$flowering10[grep(",",pd$flowering10)] <- format.Date(as.Date(pd$flowering10[grep(",",pd$flowering10)],format="%d,%m,%Y"),format="%m/%d/%Y")

##pd$flowering75 <- as.character(pd$flowering75)
#pd$flowering75[grep(",",pd$flowering75)] <- format.Date(as.Date(pd$flowering75[grep(",",pd$flowering75)],format="%d,%m,%Y"),format="%m/%d/%Y")

##pd$maturityStart <- as.character(pd$maturityStart)
#pd$maturityStart[grep(",",pd$maturityStart)] <- format.Date(as.Date(pd$maturityStart[grep(",",pd$maturityStart)],format="%d,%m,%Y"),format="%m/%d/%Y")

##pd$maturityFull <- as.character(pd$maturityFull)
#pd$maturityFull[grep(",",pd$maturityFull)] <- format.Date(as.Date(pd$maturityFull[grep(",",pd$maturityFull)],format="%d,%m,%Y"),format="%m/%d/%Y")

# 1 - белая; 2 - светло-розовая; 3- розовая; 4 - сиренево-розовая; 5 - фиолетово-розовая; 6 - красно-фиолетовая; 7 - голубая; 8  - желто - зелёная

pd[pd == "нет"] <- NA
pd[pd == "Нет растений"] <- NA
pd[pd == "нет растений"] <- NA
pd[pd == "нет всходов"] <- NA
pd[pd == "Нет всходов"] <- NA
pd[pd == ""] <- NA

#unique(pd$flowerColour)
# "нет"          "Нет растений"
# "нет всходов"  "1.2"          "7"            "нет растений"

pd$flowerColour[pd$flowerColour == "1.2"] <- 2

unique(pd$flowerColour)
unique(pd$"stemColor")
unique(pd$"bushShape")
unique(pd$"leafSize")
unique(pd$"peduncleColor")
unique(pd$"ascDamage")
unique(pd$"stemBranching")
unique(pd$"stemBranch1Length")
unique(pd$"stemBranch1BranchingType")
unique(pd$"stemBranch2BranchingType")
unique(pd$"PSH")
unique(pd$"PDH")
unique(pd$"flowering75")

ii <- !duplicated(pd[, 5])

NN <- sum(ii)

vdf <- data.frame(cbind(pd[ii, 5], pd[ii, 4], pd[ii, 3], rep(1900, NN)))
colnames(vdf) <- c('name', 'catnumber',  'origin_country', 'colyear')
SQL <- sqlAppendTable(conn, "variety", vdf, row.names = FALSE)
dbSendStatement(conn, SQL)

envdf <- data.frame(envname="KOS_2016",
		location = "KOS",
		year = "2016")

SQL <- sqlAppendTable(conn, "environment", envdf, row.names = FALSE)
dbSendStatement(conn, SQL)


apply(pd, 1, FUN=function(r) {
    for(isn in 1:5) {
	gt <- paste0("'", r[5], "'")
	dat <- gsub("NA", "NULL", paste(isn,
		format.Date(as.Date(r[6], format = "%m/%d/%Y"), format="'%Y-%m-%d'"),
		format.Date(as.Date(r[7], format = "%m/%d/%Y"), format="'%Y-%m-%d'"),
		format.Date(as.Date(r[8], format = "%m/%d/%Y"), format="'%Y-%m-%d'"),
		format.Date(as.Date(r[9], format = "%m/%d/%Y"), format="'%Y-%m-%d'"),
		format.Date(as.Date(r[10], format = "%m/%d/%Y"), format="'%Y-%m-%d'"),
		format.Date(as.Date(r[11], format = "%m/%d/%Y"), format="'%Y-%m-%d'"),
		format.Date(as.Date(r[12], format = "%m/%d/%Y"), format="'%Y-%m-%d'"),
		format.Date(as.Date(r[13], format = "%m/%d/%Y"), format="'%Y-%m-%d'"),

		r[22], r[23], r[24], r[25], r[26], r[27], r[29], r[30], r[31], r[32], r[33], r[34],
		r[34 + isn], r[39 + isn], r[44 + isn], r[49 + isn], r[54 + isn], r[59 + isn], r[64 + isn], r[70], r[70 + isn], r[75 + isn],
		r[81], r[82], r[83],

		r[2], "'KOS_2016'", sep = ','))
	qu <- paste0("INSERT INTO accession (variety, inseriesnum, sowing, seedlings10, seedlings75, flowering10, flowering75, floweringFin, maturityStart, maturityFull, flowerColor,stemColor,bushShape,leafSize,peduncleColor,ascDamage,stemBranching,stemBranch1Length,stemBranch1BranchingType,stemBranch2BranchingType,PSH,PDH,
	Ptht, Hlp, Byld, WpWp, PPP, SPP, SYDS, PodSH, PDL, PDW,
	SSH, SCO, TSW, spot, env) VALUES (", gt, ',', dat, ")")
	cat(qu, '\n')
	dbSendStatement(conn, qu)
    }
})


#apply(vdf, 1, FUN=function(r) {
#    qu <- paste0("delete from accession where variety = '", r[1], "'")
#    dbSendStatement(conn, qu)
#})


pd <- read.csv('../chickpea-eric/collection_chickpea.csv', header = 1, stringsAsFactors = FALSE)

ii <- !duplicated(pd[, 8])

NN <- sum(ii)

vdf <- data.frame(cbind(pd[ii, 8], pd[ii, 7], rep('Turkey', NN), rep(2000, NN), pd[ii, 15], pd[ii, 16]))
colnames(vdf) <- c('name', 'catnumber',  'origin_country', 'colyear', 'colsite', 'colsitecode')
apply(vdf, 1, FUN=function(r) {
    tryCatch(
		{
    qu <- paste0("insert into variety (name, catnumber,  origin_country, colyear, colsite, colsitecode) values ('", r[1], "',", r[2], ",'", r[3], "',", r[4], ",'", r[5], "','", r[6], "')")
    cat(qu, '\n')
    dbSendStatement(conn, qu)
},
	    error=function(cond) { message(cond); return(NULL)},
	    warning=function(cond) { message(cond); return(NULL) },
	    finally={ message("Some other message at the end") })
})


ii <- !duplicated(pd[, 3])

NN <- sum(ii)

locdf <- data.frame(cbind(pd[ii, 3], pd[ii, 1], pd[ii, 3], corrds[2:5, 2], corrds[2:5, 3]))
colnames(locdf) <- c('locname', 'country', 'region', 'lat', 'lon')
SQL <- sqlAppendTable(conn, "location", locdf, row.names = FALSE)
dbSendStatement(conn, SQL)


ii <- !duplicated(pd[, 4])

NN <- sum(ii)

envdf <- data.frame(envname=c("Flor_16", "MB_16", "Ank_14", "Ank_15", "Urfa_14", "Urfa_15"),
		location = c("Floreat", "Mt. Barker", "Ankara", "Ankara", "Urfa", "Urfa"),
		year = c(2016, 2016, 2014, 2015, 2014, 2015))

SQL <- sqlAppendTable(conn, "environment", envdf, row.names = FALSE)
dbSendStatement(conn, SQL)

#apply(pd[4582:5769,], 1, FUN=function(r) {
apply(pd, 1, FUN=function(r) {
    gt <- paste0("'", r[8], "'")
    tmnt <- 0
    if (tolower(r[5]) == 'drought') {
	tmnt <- 1
    } else if (tolower(r[5]) == 'ww') {
	tmnt <- 2
    } else if (tolower(r[5]) == 'tos 1') {
	tmnt <- 3
    } else if (tolower(r[5]) == 'tos 2') {
	tmnt <- 4
    } else if (tolower(r[5]) == 'tos 3') {
	tmnt <- 5
    }
    dat <- gsub("NA", "NULL", paste(r[21],
		format.Date(as.Date(r[29], format = "%d.%m.%Y"), format="'%Y-%m-%d'"),
		format.Date(as.Date(r[31], format = "%d.%m.%Y"), format="'%Y-%m-%d'"),
		tmnt,
		paste0("'", r[22], "'"), paste0("'", r[4], "'"), sep = ','))
    qu <- paste0("INSERT INTO accession (variety, inseriesnum, sowing, flowering10, treatment_id, spot, env) VALUES (", gt, ',', dat, ")")
    cat(qu, '\n')
    dbSendStatement(conn, qu)
})

#ankara-17130.01.01.2014.31.12.2016.1.0.0.en.ansi.00000000.csv

kgl <- read.csv('../chickpea-eric/ankara-17130.01.01.2014.31.12.2016.1.0.0.en.ansi.00000000.csv', comment.char="#", header = TRUE, row.names = NULL, sep = ";")

#NL <- sum(nlevels(kgl$DD),
#		nlevels(kgl$N),
#		nlevels(kgl$WW),
#		nlevels(kgl$W1),
#		nlevels(kgl$W2),
#		nlevels(kgl$Cl),
#		nlevels(kgl$Nh),
#		nlevels(kgl$H),
#		nlevels(kgl$Cm),
#		nlevels(kgl$Ch),
#		nlevels(kgl$E),
#		nlevels(kgl$E.))
#
#SQL <- sqlAppendTable(conn, "rp5levels", rp5lev, row.names = FALSE)
#dbSendStatement(conn, SQL)

#SQL <- sqlAppendTable(conn, "rp5levels_dd", data.frame(level = c(seq(1:nlevels(kgl$DD))), explanation = c(levels(kgl$DD))), row.names = FALSE)
#dbSendStatement(conn, SQL)
#SQL <- sqlAppendTable(conn, "rp5levels_n", data.frame(level = c(seq(1:nlevels(kgl$N))), explanation = c(levels(kgl$N))), row.names = FALSE)
#dbSendStatement(conn, SQL)
#SQL <- sqlAppendTable(conn, "rp5levels_ww", data.frame(level = c(seq(1:nlevels(kgl$WW))), explanation = c(levels(kgl$WW))), row.names = FALSE)
#dbSendStatement(conn, SQL)
#SQL <- sqlAppendTable(conn, "rp5levels_w1", data.frame(level = c(seq(1:nlevels(kgl$W1))), explanation = c(levels(kgl$W1))), row.names = FALSE)
#dbSendStatement(conn, SQL)
#SQL <- sqlAppendTable(conn, "rp5levels_w2", data.frame(level = c(seq(1:nlevels(kgl$W2))), explanation = c(levels(kgl$W2))), row.names = FALSE)
#dbSendStatement(conn, SQL)
#SQL <- sqlAppendTable(conn, "rp5levels_cl", data.frame(level = c(seq(1:nlevels(kgl$Cl))), explanation = c(levels(kgl$Cl))), row.names = FALSE)
#dbSendStatement(conn, SQL)
#SQL <- sqlAppendTable(conn, "rp5levels_nh", data.frame(level = c(seq(1:nlevels(kgl$Nh))), explanation = c(levels(kgl$Nh))), row.names = FALSE)
#dbSendStatement(conn, SQL)
#SQL <- sqlAppendTable(conn, "rp5levels_h", data.frame(level = c(seq(1:nlevels(kgl$H))), explanation = c(levels(kgl$H))), row.names = FALSE)
#dbSendStatement(conn, SQL)
#SQL <- sqlAppendTable(conn, "rp5levels_cm", data.frame(level = c(seq(1:nlevels(kgl$Cm))), explanation = c(levels(kgl$Cm))), row.names = FALSE)
#dbSendStatement(conn, SQL)
#SQL <- sqlAppendTable(conn, "rp5levels_ch", data.frame(level = c(seq(1:nlevels(kgl$Ch))), explanation = c(levels(kgl$Ch))), row.names = FALSE)
#dbSendStatement(conn, SQL)
#SQL <- sqlAppendTable(conn, "rp5levels_e", data.frame(level = c(seq(1:nlevels(kgl$E))), explanation = c(levels(kgl$E))), row.names = FALSE)
#dbSendStatement(conn, SQL)
#SQL <- sqlAppendTable(conn, "rp5levels_e1", data.frame(level = c(seq(1:nlevels(kgl$E.))), explanation = c(levels(kgl$E.))), row.names = FALSE)
#dbSendStatement(conn, SQL)

#odb.insert(ODB, "rp5levels", rp5lev, execute = TRUE, dateFormat = "%m/%d/%Y")

#kgl$DD <- as.numeric(kgl$DD)
kgl$N <- as.numeric(kgl$N)
kgl$WW <- as.numeric(kgl$WW)
kgl$W1 <- as.numeric(kgl$W1)
kgl$W2 <- as.numeric(kgl$W2)
kgl$Cl <- as.numeric(kgl$Cl)
kgl$Nh <- as.numeric(kgl$Nh)
kgl$H <- as.numeric(kgl$H)
kgl$Cm <- as.numeric(kgl$Cm)
kgl$Ch <- as.numeric(kgl$Ch)
#kgl$E <- as.numeric(kgl$E)
kgl$E. <- as.numeric(kgl$E.)

#kgl[,1] <- as.character(kgl[,1])
kgl[,1] <- paste("'",strptime(kgl[,1], format = "%d.%m.%Y%t%R"), "'", sep = "")

kgl$sss <- as.character(kgl$sss)
kgl$sss[kgl$sss == ""] <- 0
kgl$sss[kgl$sss == "Менее 0.5"] <- 0
kgl$sss <- as.numeric(kgl$sss)

kgl[,1] <- gsub("'", "", kgl[,1])

#kgl$RRR <- as.character(kgl$RRR)
#kgl$RRR[kgl$RRR == ""] <- 0
#kgl$RRR[kgl$RRR == "Осадков нет"] <- 0
#kgl$RRR[kgl$RRR == "Следы осадков"] <- 0.0000000000000001
#gl$RRR <- as.numeric(kgl$RRR)

kdf <- data.frame(cbind((4091+1):(4091+nrow(kgl)), rep('Ankara', nrow(kgl)), kgl[-2], daylength(corrds[corrds$locname == "Ankara", 2], kgl[, 1])))
colnames(kdf) <- c("id", "location", "tsp", tolower(gsub(".", "1", colnames(kgl)[c(-1, -2)], fixed = TRUE)), "dl")
kdf$ff3 <- as.numeric(kdf$ff3)
kdf$u <- as.numeric(kdf$u)
kdf$tx <- as.numeric(kdf$tx)
kdf$td <- as.numeric(kdf$td)
kdf$tg <- as.numeric(kdf$tg)
kdf$tr <- as.numeric(kdf$tr)
kdf[kdf$ch == 13,]$ch = 10
kdf[kdf$ch == 26,]$ch = 2
kdf[kdf$ch == 25,]$ch = 5
kdf[kdf$ch == 27,]$ch = 7
kdf[kdf$ch == 22,]$ch = 2
kdf$ch = 2
kdf[kdf$n == 35,]$n = 3
kdf[kdf$n == 62,]$n = 6
kdf$w2 = 2
kdf$e1 = 1
kdf$e = 1
kdf$n = 1
kdf$nh = 1
kdf$dd = 1
SQL <- sqlAppendTable(conn, "rp5data", kdf, row.names = FALSE)
dbSendStatement(conn, SQL)

#barker-94675.01.01.2014.31.12.2016.1.0.0.en.ansi.00000000.csv

kgl <- read.csv('../chickpea-eric/barker-94675.01.01.2014.31.12.2016.1.0.0.en.ansi.00000000.csv', comment.char="#", header = TRUE, row.names = NULL, sep = ";")

#NL <- sum(nlevels(kgl$DD),
#		nlevels(kgl$N),
#		nlevels(kgl$WW),
#		nlevels(kgl$W1),
#		nlevels(kgl$W2),
#		nlevels(kgl$Cl),
#		nlevels(kgl$Nh),
#		nlevels(kgl$H),
#		nlevels(kgl$Cm),
#		nlevels(kgl$Ch),
#		nlevels(kgl$E),
#		nlevels(kgl$E.))
#
#SQL <- sqlAppendTable(conn, "rp5levels", rp5lev, row.names = FALSE)
#dbSendStatement(conn, SQL)

#SQL <- sqlAppendTable(conn, "rp5levels_dd", data.frame(level = c(seq(1:nlevels(kgl$DD))), explanation = c(levels(kgl$DD))), row.names = FALSE)
#dbSendStatement(conn, SQL)
#SQL <- sqlAppendTable(conn, "rp5levels_n", data.frame(level = c(seq(1:nlevels(kgl$N))), explanation = c(levels(kgl$N))), row.names = FALSE)
#dbSendStatement(conn, SQL)
#SQL <- sqlAppendTable(conn, "rp5levels_ww", data.frame(level = c(seq(1:nlevels(kgl$WW))), explanation = c(levels(kgl$WW))), row.names = FALSE)
#dbSendStatement(conn, SQL)
#SQL <- sqlAppendTable(conn, "rp5levels_w1", data.frame(level = c(seq(1:nlevels(kgl$W1))), explanation = c(levels(kgl$W1))), row.names = FALSE)
#dbSendStatement(conn, SQL)
#SQL <- sqlAppendTable(conn, "rp5levels_w2", data.frame(level = c(seq(1:nlevels(kgl$W2))), explanation = c(levels(kgl$W2))), row.names = FALSE)
#dbSendStatement(conn, SQL)
#SQL <- sqlAppendTable(conn, "rp5levels_cl", data.frame(level = c(seq(1:nlevels(kgl$Cl))), explanation = c(levels(kgl$Cl))), row.names = FALSE)
#dbSendStatement(conn, SQL)
#SQL <- sqlAppendTable(conn, "rp5levels_nh", data.frame(level = c(seq(1:nlevels(kgl$Nh))), explanation = c(levels(kgl$Nh))), row.names = FALSE)
#dbSendStatement(conn, SQL)
#SQL <- sqlAppendTable(conn, "rp5levels_h", data.frame(level = c(seq(1:nlevels(kgl$H))), explanation = c(levels(kgl$H))), row.names = FALSE)
#dbSendStatement(conn, SQL)
#SQL <- sqlAppendTable(conn, "rp5levels_cm", data.frame(level = c(seq(1:nlevels(kgl$Cm))), explanation = c(levels(kgl$Cm))), row.names = FALSE)
#dbSendStatement(conn, SQL)
#SQL <- sqlAppendTable(conn, "rp5levels_ch", data.frame(level = c(seq(1:nlevels(kgl$Ch))), explanation = c(levels(kgl$Ch))), row.names = FALSE)
#dbSendStatement(conn, SQL)
#SQL <- sqlAppendTable(conn, "rp5levels_e", data.frame(level = c(seq(1:nlevels(kgl$E))), explanation = c(levels(kgl$E))), row.names = FALSE)
#dbSendStatement(conn, SQL)
#SQL <- sqlAppendTable(conn, "rp5levels_e1", data.frame(level = c(seq(1:nlevels(kgl$E.))), explanation = c(levels(kgl$E.))), row.names = FALSE)
#dbSendStatement(conn, SQL)

#odb.insert(ODB, "rp5levels", rp5lev, execute = TRUE, dateFormat = "%m/%d/%Y")

#kgl$DD <- as.numeric(kgl$DD)
kgl$N <- as.numeric(kgl$N)
kgl$WW <- as.numeric(kgl$WW)
kgl$W1 <- as.numeric(kgl$W1)
kgl$W2 <- as.numeric(kgl$W2)
kgl$Cl <- as.numeric(kgl$Cl)
kgl$Nh <- as.numeric(kgl$Nh)
kgl$H <- as.numeric(kgl$H)
kgl$Cm <- as.numeric(kgl$Cm)
kgl$Ch <- as.numeric(kgl$Ch)
#kgl$E <- as.numeric(kgl$E)
kgl$E. <- as.numeric(kgl$E.)

#kgl[,1] <- as.character(kgl[,1])
kgl[,1] <- paste("'",strptime(kgl[,1], format = "%d.%m.%Y%t%R"), "'", sep = "")

kgl$sss <- as.character(kgl$sss)
kgl$sss[kgl$sss == ""] <- 0
kgl$sss[kgl$sss == "Менее 0.5"] <- 0
kgl$sss <- as.numeric(kgl$sss)

kgl[,1] <- gsub("'", "", kgl[,1])

#kgl$RRR <- as.character(kgl$RRR)
#kgl$RRR[kgl$RRR == ""] <- 0
#kgl$RRR[kgl$RRR == "Осадков нет"] <- 0
#kgl$RRR[kgl$RRR == "Следы осадков"] <- 0.0000000000000001
#gl$RRR <- as.numeric(kgl$RRR)

kdf <- data.frame(cbind((29555+1):(29555+nrow(kgl)), rep('Mt. Barker', nrow(kgl)), kgl[-2], daylength(corrds[corrds$locname == "Mt. Barker", 2], kgl[, 1])))
colnames(kdf) <- c("id", "location", "tsp", tolower(gsub(".", "1", colnames(kgl)[c(-1, -2)], fixed = TRUE)), "dl")
kdf$ff3 <- as.numeric(kdf$ff3)
kdf$u <- as.numeric(kdf$u)
kdf$tx <- as.numeric(kdf$tx)
kdf$td <- as.numeric(kdf$td)
kdf$tg <- as.numeric(kdf$tg)
kdf$tr <- as.numeric(kdf$tr)
kdf$ch = 2
kdf$w2 = 2
kdf$e1 = 1
kdf$e = 1
kdf$n = 1
kdf$nh = 1
kdf$dd = 1
SQL <- sqlAppendTable(conn, "rp5data", kdf, row.names = FALSE)
dbSendStatement(conn, SQL)

rp5 <- dbReadTable(conn, 'rp5data')
dim(rp5)

#urfa-17270.01.01.2014.31.12.2016.1.0.0.en.ansi.00000000.csv

kgl <- read.csv('../chickpea-eric/urfa-17270.01.01.2014.31.12.2016.1.0.0.en.ansi.00000000.csv', comment.char="#", header = TRUE, row.names = NULL, sep = ";")

#NL <- sum(nlevels(kgl$DD),
#		nlevels(kgl$N),
#		nlevels(kgl$WW),
#		nlevels(kgl$W1),
#		nlevels(kgl$W2),
#		nlevels(kgl$Cl),
#		nlevels(kgl$Nh),
#		nlevels(kgl$H),
#		nlevels(kgl$Cm),
#		nlevels(kgl$Ch),
#		nlevels(kgl$E),
#		nlevels(kgl$E.))
#
#SQL <- sqlAppendTable(conn, "rp5levels", rp5lev, row.names = FALSE)
#dbSendStatement(conn, SQL)

#SQL <- sqlAppendTable(conn, "rp5levels_dd", data.frame(level = c(seq(1:nlevels(kgl$DD))), explanation = c(levels(kgl$DD))), row.names = FALSE)
#dbSendStatement(conn, SQL)
#SQL <- sqlAppendTable(conn, "rp5levels_n", data.frame(level = c(seq(1:nlevels(kgl$N))), explanation = c(levels(kgl$N))), row.names = FALSE)
#dbSendStatement(conn, SQL)
#SQL <- sqlAppendTable(conn, "rp5levels_ww", data.frame(level = c(seq(1:nlevels(kgl$WW))), explanation = c(levels(kgl$WW))), row.names = FALSE)
#dbSendStatement(conn, SQL)
#SQL <- sqlAppendTable(conn, "rp5levels_w1", data.frame(level = c(seq(1:nlevels(kgl$W1))), explanation = c(levels(kgl$W1))), row.names = FALSE)
#dbSendStatement(conn, SQL)
#SQL <- sqlAppendTable(conn, "rp5levels_w2", data.frame(level = c(seq(1:nlevels(kgl$W2))), explanation = c(levels(kgl$W2))), row.names = FALSE)
#dbSendStatement(conn, SQL)
#SQL <- sqlAppendTable(conn, "rp5levels_cl", data.frame(level = c(seq(1:nlevels(kgl$Cl))), explanation = c(levels(kgl$Cl))), row.names = FALSE)
#dbSendStatement(conn, SQL)
#SQL <- sqlAppendTable(conn, "rp5levels_nh", data.frame(level = c(seq(1:nlevels(kgl$Nh))), explanation = c(levels(kgl$Nh))), row.names = FALSE)
#dbSendStatement(conn, SQL)
#SQL <- sqlAppendTable(conn, "rp5levels_h", data.frame(level = c(seq(1:nlevels(kgl$H))), explanation = c(levels(kgl$H))), row.names = FALSE)
#dbSendStatement(conn, SQL)
#SQL <- sqlAppendTable(conn, "rp5levels_cm", data.frame(level = c(seq(1:nlevels(kgl$Cm))), explanation = c(levels(kgl$Cm))), row.names = FALSE)
#dbSendStatement(conn, SQL)
#SQL <- sqlAppendTable(conn, "rp5levels_ch", data.frame(level = c(seq(1:nlevels(kgl$Ch))), explanation = c(levels(kgl$Ch))), row.names = FALSE)
#dbSendStatement(conn, SQL)
#SQL <- sqlAppendTable(conn, "rp5levels_e", data.frame(level = c(seq(1:nlevels(kgl$E))), explanation = c(levels(kgl$E))), row.names = FALSE)
#dbSendStatement(conn, SQL)
#SQL <- sqlAppendTable(conn, "rp5levels_e1", data.frame(level = c(seq(1:nlevels(kgl$E.))), explanation = c(levels(kgl$E.))), row.names = FALSE)
#dbSendStatement(conn, SQL)

#odb.insert(ODB, "rp5levels", rp5lev, execute = TRUE, dateFormat = "%m/%d/%Y")

#kgl$DD <- as.numeric(kgl$DD)
kgl$N <- as.numeric(kgl$N)
kgl$WW <- as.numeric(kgl$WW)
kgl$W1 <- as.numeric(kgl$W1)
kgl$W2 <- as.numeric(kgl$W2)
kgl$Cl <- as.numeric(kgl$Cl)
kgl$Nh <- as.numeric(kgl$Nh)
kgl$H <- as.numeric(kgl$H)
kgl$Cm <- as.numeric(kgl$Cm)
kgl$Ch <- as.numeric(kgl$Ch)
#kgl$E <- as.numeric(kgl$E)
kgl$E. <- as.numeric(kgl$E.)

#kgl[,1] <- as.character(kgl[,1])
kgl[,1] <- paste("'",strptime(kgl[,1], format = "%d.%m.%Y%t%R"), "'", sep = "")

kgl$sss <- as.character(kgl$sss)
kgl$sss[kgl$sss == ""] <- 0
kgl$sss[kgl$sss == "Менее 0.5"] <- 0
kgl$sss <- as.numeric(kgl$sss)

kgl[,1] <- gsub("'", "", kgl[,1])

#kgl$RRR <- as.character(kgl$RRR)
#kgl$RRR[kgl$RRR == ""] <- 0
#kgl$RRR[kgl$RRR == "Осадков нет"] <- 0
#kgl$RRR[kgl$RRR == "Следы осадков"] <- 0.0000000000000001
#gl$RRR <- as.numeric(kgl$RRR)

kdf <- data.frame(cbind((38543+1):(38543+nrow(kgl)), rep('Urfa', nrow(kgl)), kgl[-2], daylength(corrds[corrds$locname == "Urfa", 2], kgl[, 1])))
colnames(kdf) <- c("id", "location", "tsp", tolower(gsub(".", "1", colnames(kgl)[c(-1, -2)], fixed = TRUE)), "dl")
kdf$ff3 <- as.numeric(kdf$ff3)
kdf$u <- as.numeric(kdf$u)
kdf$tx <- as.numeric(kdf$tx)
kdf$td <- as.numeric(kdf$td)
kdf$tg <- as.numeric(kdf$tg)
kdf$tr <- as.numeric(kdf$tr)
kdf$ch = 2
kdf$w2 = 2
kdf$e1 = 1
kdf$e = 1
kdf$n = 1
kdf$nh = 1
kdf$dd = 1
SQL <- sqlAppendTable(conn, "rp5data", kdf, row.names = FALSE)
dbSendStatement(conn, SQL)

rp5 <- dbReadTable(conn, 'rp5data')
dim(rp5)

#perth-94608.01.01.2014.31.12.2016.1.0.0.en.ansi.00000000.csv

kgl <- read.csv('../chickpea-eric/perth-94608.01.01.2014.31.12.2016.1.0.0.en.ansi.00000000.csv', comment.char="#", header = TRUE, row.names = NULL, sep = ";")

#NL <- sum(nlevels(kgl$DD),
#		nlevels(kgl$N),
#		nlevels(kgl$WW),
#		nlevels(kgl$W1),
#		nlevels(kgl$W2),
#		nlevels(kgl$Cl),
#		nlevels(kgl$Nh),
#		nlevels(kgl$H),
#		nlevels(kgl$Cm),
#		nlevels(kgl$Ch),
#		nlevels(kgl$E),
#		nlevels(kgl$E.))
#
#SQL <- sqlAppendTable(conn, "rp5levels", rp5lev, row.names = FALSE)
#dbSendStatement(conn, SQL)

#SQL <- sqlAppendTable(conn, "rp5levels_dd", data.frame(level = c(seq(1:nlevels(kgl$DD))), explanation = c(levels(kgl$DD))), row.names = FALSE)
#dbSendStatement(conn, SQL)
#SQL <- sqlAppendTable(conn, "rp5levels_n", data.frame(level = c(seq(1:nlevels(kgl$N))), explanation = c(levels(kgl$N))), row.names = FALSE)
#dbSendStatement(conn, SQL)
#SQL <- sqlAppendTable(conn, "rp5levels_ww", data.frame(level = c(seq(1:nlevels(kgl$WW))), explanation = c(levels(kgl$WW))), row.names = FALSE)
#dbSendStatement(conn, SQL)
#SQL <- sqlAppendTable(conn, "rp5levels_w1", data.frame(level = c(seq(1:nlevels(kgl$W1))), explanation = c(levels(kgl$W1))), row.names = FALSE)
#dbSendStatement(conn, SQL)
#SQL <- sqlAppendTable(conn, "rp5levels_w2", data.frame(level = c(seq(1:nlevels(kgl$W2))), explanation = c(levels(kgl$W2))), row.names = FALSE)
#dbSendStatement(conn, SQL)
#SQL <- sqlAppendTable(conn, "rp5levels_cl", data.frame(level = c(seq(1:nlevels(kgl$Cl))), explanation = c(levels(kgl$Cl))), row.names = FALSE)
#dbSendStatement(conn, SQL)
#SQL <- sqlAppendTable(conn, "rp5levels_nh", data.frame(level = c(seq(1:nlevels(kgl$Nh))), explanation = c(levels(kgl$Nh))), row.names = FALSE)
#dbSendStatement(conn, SQL)
#SQL <- sqlAppendTable(conn, "rp5levels_h", data.frame(level = c(seq(1:nlevels(kgl$H))), explanation = c(levels(kgl$H))), row.names = FALSE)
#dbSendStatement(conn, SQL)
#SQL <- sqlAppendTable(conn, "rp5levels_cm", data.frame(level = c(seq(1:nlevels(kgl$Cm))), explanation = c(levels(kgl$Cm))), row.names = FALSE)
#dbSendStatement(conn, SQL)
#SQL <- sqlAppendTable(conn, "rp5levels_ch", data.frame(level = c(seq(1:nlevels(kgl$Ch))), explanation = c(levels(kgl$Ch))), row.names = FALSE)
#dbSendStatement(conn, SQL)
#SQL <- sqlAppendTable(conn, "rp5levels_e", data.frame(level = c(seq(1:nlevels(kgl$E))), explanation = c(levels(kgl$E))), row.names = FALSE)
#dbSendStatement(conn, SQL)
#SQL <- sqlAppendTable(conn, "rp5levels_e1", data.frame(level = c(seq(1:nlevels(kgl$E.))), explanation = c(levels(kgl$E.))), row.names = FALSE)
#dbSendStatement(conn, SQL)

#odb.insert(ODB, "rp5levels", rp5lev, execute = TRUE, dateFormat = "%m/%d/%Y")

#kgl$DD <- as.numeric(kgl$DD)
kgl$N <- as.numeric(kgl$N)
kgl$WW <- as.numeric(kgl$WW)
kgl$W1 <- as.numeric(kgl$W1)
kgl$W2 <- as.numeric(kgl$W2)
kgl$Cl <- as.numeric(kgl$Cl)
kgl$Nh <- as.numeric(kgl$Nh)
kgl$H <- as.numeric(kgl$H)
kgl$Cm <- as.numeric(kgl$Cm)
kgl$Ch <- as.numeric(kgl$Ch)
#kgl$E <- as.numeric(kgl$E)
kgl$E. <- as.numeric(kgl$E.)

#kgl[,1] <- as.character(kgl[,1])
kgl[,1] <- paste("'",strptime(kgl[,1], format = "%d.%m.%Y%t%R"), "'", sep = "")

kgl$sss <- as.character(kgl$sss)
kgl$sss[kgl$sss == ""] <- 0
kgl$sss[kgl$sss == "Менее 0.5"] <- 0
kgl$sss <- as.numeric(kgl$sss)

kgl[,1] <- gsub("'", "", kgl[,1])

#kgl$RRR <- as.character(kgl$RRR)
#kgl$RRR[kgl$RRR == ""] <- 0
#kgl$RRR[kgl$RRR == "Осадков нет"] <- 0
#kgl$RRR[kgl$RRR == "Следы осадков"] <- 0.0000000000000001
#gl$RRR <- as.numeric(kgl$RRR)

kdf <- data.frame(cbind((56290+1):(56290+nrow(kgl)), rep('Floreat', nrow(kgl)), kgl[-2], daylength(corrds[corrds$locname == "Floreat", 2], kgl[, 1])))
colnames(kdf) <- c("id", "location", "tsp", tolower(gsub(".", "1", colnames(kgl)[c(-1, -2)], fixed = TRUE)), "dl")
kdf$ff3 <- as.numeric(kdf$ff3)
kdf$u <- as.numeric(kdf$u)
kdf$tx <- as.numeric(kdf$tx)
kdf$td <- as.numeric(kdf$td)
kdf$tg <- as.numeric(kdf$tg)
kdf$tr <- as.numeric(kdf$tr)
kdf$ch = 2
kdf$w2 = 2
kdf$e1 = 1
kdf$e = 1
kdf$n = 1
kdf$nh = 1
kdf$dd = 1
SQL <- sqlAppendTable(conn, "rp5data", kdf, row.names = FALSE)
dbSendStatement(conn, SQL)


rp5 <- dbReadTable(conn, 'rp5data')
dim(rp5)


