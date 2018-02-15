SQL <- c(
	"CREATE TABLE variety (
		name VARCHAR(64) PRIMARY KEY,
		catnumber VARCHAR(32),
		origin VARCHAR(64),
		colyear INTEGER
	)"
)

dbSendStatement(conn, SQL)

SQL <- c(
	"CREATE TABLE location (
		locname VARCHAR(64) PRIMARY KEY,
		country VARCHAR(64),
		region VARCHAR(64)
	)"
)

dbSendStatement(conn, SQL)

SQL <- c(
	"CREATE TABLE environment (
		envname VARCHAR(64) PRIMARY KEY,
		location VARCHAR(64),
		year INTEGER
	)"
)

dbSendStatement(conn, SQL)

SQL <- c(
	"ALTER TABLE environment ADD FOREIGN KEY (location) REFERENCES location(locname)"
)

dbSendStatement(conn, SQL)

SQL <- c(
	"CREATE TABLE snp (
		snpabbr VARCHAR(64) PRIMARY KEY,
		chrname VARCHAR(80),
		pos INTEGER,
		ref VARCHAR(1),
		A INTEGER,
		C INTEGER,
		G INTEGER,
		T INTEGER,
		quality FLOAT
	)"
)

dbSendStatement(conn, SQL)

SQL <- c(
	"CREATE TABLE genotype (
		gname VARCHAR(64) PRIMARY KEY,
		ancleft VARCHAR(64),
		ancright VARCHAR(64)
	)"
)

dbSendStatement(conn, SQL)

SQL <- c(
	"CREATE TABLE gt_snp (
		id SERIAL PRIMARY KEY,
		gt VARCHAR(64),
		snpabbr VARCHAR(64),
		ndom INTEGER,
		ref INTEGER,
		alt1 INTEGER,
		alt2 INTEGER,
		alt3 INTEGER
	)"
)

dbSendStatement(conn, SQL)

SQL <- c(
	'ALTER TABLE gt_snp ADD CONSTRAINT "CHECK_ndom" CHECK (ndom >= 0 AND ndom <= 2)'
)

dbSendStatement(conn, SQL)

SQL <- c(
	"CREATE TABLE cnum (
		cnabbr VARCHAR(64) PRIMARY KEY,
		chrname VARCHAR(64),
		chrnumber INTEGER,
		coordstart INTEGER,
		coordend INTEGER,
		length INTEGER
	)"
)

dbSendStatement(conn, SQL)

SQL <- c(
	"CREATE TABLE gt_cnum (
		id SERIAL PRIMARY KEY,
		gt VARCHAR(64),
		cnabbr VARCHAR(64),
		cnumber INTEGER
	)"
)

dbSendStatement(conn, SQL)

SQL <- c(
	'ALTER TABLE gt_cnum ADD CONSTRAINT "CHECK_cnumber" CHECK (cnumber >= 0 AND cnumber <= 2)'
)

dbSendStatement(conn, SQL)

SQL <- c(
	"ALTER TABLE gt_snp ADD FOREIGN KEY (gt) REFERENCES genotype(gname)"
)

dbSendStatement(conn, SQL)

SQL <- c(
	"ALTER TABLE gt_snp ADD FOREIGN KEY (snpabbr) REFERENCES snp(snpabbr)"
)

dbSendStatement(conn, SQL)

SQL <- c(
	"ALTER TABLE gt_cnum ADD FOREIGN KEY (gt) REFERENCES genotype(gname)"
)

dbSendStatement(conn, SQL)

SQL <- c(
	"ALTER TABLE gt_cnum ADD FOREIGN KEY (cnabbr) REFERENCES cnum(cnabbr)"
)

dbSendStatement(conn, SQL)

SQL <- c(
	"ALTER TABLE genotype ADD FOREIGN KEY (ancleft) REFERENCES variety(name)"
)

dbSendStatement(conn, SQL)

SQL <- c(
	"ALTER TABLE genotype ADD FOREIGN KEY (ancright) REFERENCES variety(name)"
)

dbSendStatement(conn, SQL)

SQL <- c(
	"CREATE TABLE crop_names (
		id SERIAL PRIMARY KEY,
		short_name VARCHAR(64),
		latin_name VARCHAR(64),
		russian_name VARCHAR(64)
	)"
)

dbSendStatement(conn, SQL)

SQL <- c(
	"CREATE TABLE accession (
		num SERIAL PRIMARY KEY,
		genotype VARCHAR(64),
		variety VARCHAR(64),
		inseriesnum INTEGER,
		sowing DATE,
		seedlings10 DATE,
		seedlings75 DATE,
		flowering10 DATE,
		flowering75 DATE,
		floweringFin DATE,
		maturityStart DATE,
		maturityFull DATE,
		flowerColor INTEGER,
		stemColor INTEGER,
		bushShape INTEGER,
		leafSize INTEGER,
		peduncleColor INTEGER,
		ascDamage INTEGER,
		stemBranchning INTEGER,
		stemBranch1Length INTEGER,
		stemBranch1BranchingType INTEGER,
		stemBranch2BranchingType INTEGER,
		beanBeadiness INTEGER,
		beanCracking INTEGER,
		Ptht_1 FLOAT,
		Ptht_2 FLOAT,
		Ptht_3 FLOAT,
		Ptht_4 FLOAT,
		Ptht_5 FLOAT,
		Hlp_1 FLOAT,
		Hlp_2 FLOAT,
		Hlp_3 FLOAT,
		Hlp_4 FLOAT,
		Hlp_5 FLOAT,
		Byld_1 FLOAT,
		Byld_2 FLOAT,
		Byld_3 FLOAT,
		Byld_4 FLOAT,
		Byld_5 FLOAT,
		WpWp_1 FLOAT,
		WpWp_2 FLOAT,
		WpWp_3 FLOAT,
		WpWp_4 FLOAT,
		WpWp_5 FLOAT,
		PPP_1 INTEGER,
		PPP_2 INTEGER,
		PPP_3 INTEGER,
		PPP_4 INTEGER,
		PPP_5 INTEGER,
		SPP_1 INTEGER,
		SPP_2 INTEGER,
		SPP_3 INTEGER,
		SPP_4 INTEGER,
		SPP_5 INTEGER,
		SPD_1 INTEGER,
		SYDP_1 FLOAT,
		SYDS_1 FLOAT,
		SYDS_2 FLOAT,
		SYDS_3 FLOAT,
		SYDS_4 FLOAT,
		SYDS_5 FLOAT,
		PodSH INTEGER,
		PodPed INTEGER,
		PDL_1 FLOAT,
		PDL_2 FLOAT,
		PDL_3 FLOAT,
		PDL_4 FLOAT,
		PDL_5 FLOAT,
		PDW_1 FLOAT,
		PDW_2 FLOAT,
		PDW_3 FLOAT,
		PDW_4 FLOAT,
		PDW_5 FLOAT,
		PDH INTEGER,
		SSH INTEGER,
		SCO INTEGER,
		TSW FLOAT,
		env VARCHAR(64),
		spot VARCHAR(64),
		crop_name_id INTEGER
	)"
)

dbSendStatement(conn, SQL)

SQL <- c(
	"ALTER TABLE accession ADD FOREIGN KEY (genotype) REFERENCES genotype(gname)"
)

dbSendStatement(conn, SQL)

SQL <- c(
	"ALTER TABLE accession ADD FOREIGN KEY (variety) REFERENCES variety(name)"
)

dbSendStatement(conn, SQL)

SQL <- c(
	"ALTER TABLE accession ADD FOREIGN KEY (env) REFERENCES environment(envname)"
)

dbSendStatement(conn, SQL)

SQL <- c(
	"ALTER TABLE accession ADD FOREIGN KEY (crop_name_id) REFERENCES crop_names(id)"
)

dbSendStatement(conn, SQL)

SQL <- c(
	"CREATE INDEX gtindex ON accession (genotype)"
)

dbSendStatement(conn, SQL)

SQL <- c(
	"CREATE INDEX vrindex ON accession (variety)"
)

dbSendStatement(conn, SQL)


SQL <- c(
	"CREATE INDEX crindex ON accession (crop_name_id)"
)

dbSendStatement(conn, SQL)


SQL <- c(
	"CREATE TABLE accession_metadata (
		id SERIAL PRIMARY KEY,
		name VARCHAR(64),
		explanation VARCHAR(4096),
		units VARCHAR(64)
	)"
)

dbSendStatement(conn, SQL)

accession_meta <- data.frame(id = 1:65, name = c(
	'flowerColor',  # 1
	'stemColor',  # 2
	'bushShape',  # 3
	'leafSize',  # 4
	'peduncleColor',  # 5
	'ascDamage',  # 6
	'stemBranchning',  # 7
	'stemBranch1Length',  # 8
	'stemBranch1BranchingType',  # 9
	'stemBranch2BranchingType',  # 10
	'PSH',  # 11 beanBeadiness
	'Ptht',  # 13
	'Hlp',  # 18
	'Byld',  # 23
	'WpWp',  # 28
	'PPP',  # 33
	'SPP',  # 38
	'SPD',  # 43
	'SYDP',  # 44
	'SYDS',  # 45
	'PodSH',  # 50
	'PodPed',  # 51
	'PDL',  # 52
	'PDW',  # 57
	'PDH',  # 62
	'SSH',  # 63
	'SCO',  # 64
	'TSW'   # 65
	),
		explanation = c(
			'Цветок - окраска, балл. 1 - белая; 2 - светло-розовая; 3- розовая; 4 - сиренево-розовая; 5 - фиолетово-розовая; 6 - красно-фиолетовая; 7 - голубая; 8  - желто - зелёная',
			'Стебель окраска, балл. 1 - белая; 2 - светло-розовая; 3- розовая; 4 - сиренево-розовая; 5 - фиолетово-розовая; 6 - красно-фиолетовая; 7 - голубая; 8  - желто - зелёная',
			'Куст - форма, балл. 1 - стелющаяся; 3 - развалистая; 5 - стоячая (раскидистая вверху); 7 - стоячая (комактная)',
			'Лист - размер листочков (балл). 1 - очень мелкие (<9 мм); 3- мелкие (9 - 11);   5 - средние (12-15); 7 - крупные (16-20);  9 - очень крупные (> 20)',
			'Цветоножка (окраска), балл. 3 - зелёная; 7 - антоциановая',
			'Аскохитоз (поражение), балл. 1- очень слабое; 3 - слабое; 5 - среднее; 7- сильное; 9-очень сильное (эта шкала применяется при ежегодной полевой оценке).',
			'Стебель- ветвистость (балл). 1 - очень слабая (1-2); 3 - слабая (2-3); 5 - средняя (3-4); 7 - сильная (4-5); 9 - очень сильная (> 5)',
			'Стебель -  длина ветвей 1-го порядка, балл. 3 - короче; 5 - равны; 7 - длиннее',
			'Стебель - характер ветвления (ветви 1-го порядка), балл. 1 - прикорневое; 3 - в нижней половине стебля; 5 - в верхней половине стебля; 7 - по всему стеблю',
			'Стебель - характер ветвления (ветви 2-го порядка), балл. 1 - в нижнем ярусе; 3 - в среднем ярусе; 5 - в верхнем ярусе; 7 -  во всех ярусах',
			'Осыпаемость бобов, балл. 1 <  10%; 2 > 10%',
			'Высота растений (см)',
			'Стебель -высота прикрепления нижнего боба, см',
			'Вес всего растения (с бобами), г',
			'Вес растения без бобов, г',
			'Число бобов с 1-го растения, шт',
			'Число семян с 1-го растения',
			'Число семян в бобе',
			'Вес семян с делянки, г',
			'Вес семян с 1-го растения, г',
			'Боб - форма, балл. 3 - удлинённо-овальная; 5 - грушевидная; 7 - ромбическая',
			'Число бобов на плодоножке',
			'Боб - длина, мм',
			'Боб - ширина боба, мм',
			'Растрескиваемость бобов, балл. 1 <  10%;  2 > 10%',
			'Семя - форма, балл. 3 - угловатая (голова барана); 5 - промежуточная (голова совы); 7 - гороховидная',
			'Семя - окраска семенной кожуры, балл. 1 - белая; 2 - желто-розовая; 3- розовая; 4 - желтая; 5 - серая; 6 - темно-зеленая; 7 - светло-зеленая; 8  - оранжевая; 9 - рыжая; 10 - коричневая; 11 - светло-коричневая; 12 - красно-коричневая; 13 красно-фиолетовая; 14 - черная ',
			'Масса 1000 семян, г'),
		units = c(
			'INTEGER',  # 1
			'INTEGER',  # 2
			'INTEGER',  # 3
			'INTEGER',  # 4
			'INTEGER',  # 5
			'INTEGER',  # 6
			'INTEGER',  # 7
			'INTEGER',  # 8
			'INTEGER',  # 9
			'INTEGER',  # 10
			'INTEGER',  # 11
			'FLOAT',  # 13
			'FLOAT',  # 18
			'FLOAT',  # 23
			'FLOAT',  # 28
			'INTEGER',  # 33
			'INTEGER',  # 38
			'INTEGER',  # 43
			'FLOAT',  # 44
			'FLOAT',  # 45
			'INTEGER',  # 50
			'INTEGER',  # 51
			'FLOAT',  # 52
			'FLOAT',  # 57
			'INTEGER',  # 62
			'INTEGER',  # 63
			'INTEGER',  # 64
			'FLOAT'   # 65
		)
)

SQL <- sqlAppendTable(conn, "accession_metadata", accession_meta, row.names = FALSE)
dbSendStatement(conn, SQL)

SQL <- c(
	"CREATE TABLE meteo (
		id SERIAL PRIMARY KEY,
		location VARCHAR(64),
		year INTEGER,
		month INTEGER,
		Tmean FLOAT,
		Pmean FLOAT,
		Pdays FLOAT,
		Tsum10 FLOAT,
		Tsoil FLOAT,
		Hrel FLOAT
	)"
)

dbSendStatement(conn, SQL)

SQL <- c(
	"ALTER TABLE meteo ADD FOREIGN KEY (location) REFERENCES location(locname)"
)

dbSendStatement(conn, SQL)

SQL <- c(
	"CREATE VIEW sowing_to_seedlings10
	 as
	 SELECT genotype, inseriesnum,
			seedlings10 - sowing as sowingToSeedlings10,
			Tmean as seedlings10Tmean,
			Pmean as seedlings10Pmean,
			Pdays seedlings10Pdays,
			Tsum10 as seedlings10Tsum10,
			Tsoil as seedlings10Tsoil,
			Hrel as seedlings10Hrel
			FROM accession, meteo
			where month = EXTRACT(MONTH from seedlings10) AND year = EXTRACT(YEAR from seedlings10)"
)

dbSendStatement(conn, SQL)

SQL <- c(
	"CREATE VIEW sowing_to_seedlings75
	 as
	 SELECT genotype, inseriesnum,
			seedlings10 - sowing as sowingToSeedlings75,
			Tmean as seedlings75Tmean,
			Pmean as seedlings75Pmean,
			Pdays seedlings75Pdays,
			Tsum10 as seedlings75Tsum10,
			Tsoil as seedlings75Tsoil,
			Hrel as seedlings75Hrel
			FROM accession, meteo
			where month = EXTRACT(MONTH from seedlings75) AND year = EXTRACT(YEAR from seedlings75)"
)

dbSendStatement(conn, SQL)

SQL <- c(
	"CREATE VIEW seedlings75_to_flowering10
	 as
	 SELECT genotype, inseriesnum,
			flowering10 - seedlings75 as seedlings75ToFlowering10,
			Tmean as flowering10Tmean,
			Pmean as flowering10Pmean,
			Pdays flowering10Pdays,
			Tsum10 as flowering10Tsum10,
			Tsoil as flowering10Tsoil,
			Hrel as flowering10Hrel
			FROM accession, meteo
			where month = EXTRACT(MONTH from flowering10) AND year = EXTRACT(YEAR from flowering10)"
)

dbSendStatement(conn, SQL)

SQL <- c(
	"CREATE VIEW flowering75_to_maturityFull
	 as
	 SELECT genotype, inseriesnum,
			maturityFull - flowering75 as flowering75ToMaturityFull,
			Tmean as maturityFullTmean,
			Pmean as maturityFullPmean,
			Pdays maturityFull10Pdays,
			Tsum10 as maturityFullTsum10,
			Tsoil as maturityFullTsoil,
			Hrel as maturityFull10Hrel
			FROM accession, meteo
			where month = EXTRACT(MONTH from maturityFull) AND year = EXTRACT(YEAR from maturityFull)"
)

dbSendStatement(conn, SQL)

SQL <- c(
	"CREATE TABLE rp5metadata (
		id SERIAL PRIMARY KEY,
		name VARCHAR(64),
		explanation VARCHAR(4096),
		units VARCHAR(64)
	)"
)

dbSendStatement(conn, SQL)

rp5meta <- data.frame(id = 1:28, name = c('T',
		'Po',
		'P',
		'Pa',
		'U',
		'DD',
		'Ff',
		'ff10',
		'ff3',
		'N',
		'WW',
		'W1',
		'W2',
		'Tn',
		'Tx',
		'Cl',
		'Nh',
		'H',
		'Cm',
		'Ch',
		'VV',
		'Td',
		'RRR',
		'tR',
		'E',
		'Tg',
		'E1',
		'sss'),
		explanation = c('Температура воздуха над поверхностью земли 2 м',
		'Давление на уровне станции',
		'Давление, приведенное к среднему уровню моря',
		'Барическая разница - изменение давления за 3 часа',
		'Относительная влажность на высоте 2 м',
		'Направление ветра (румбы) на высоте 10-12 м, осрелненное за предыдущие 10 мин',
		'Скорость ветра на высоте 10-12 м, осрелненное за предыдущие 10 мин',
		'Максимальное значение порыва ветра на высоте 10-12 м за предыдущие 10 мин',
		'Максимальное значение порыва ветра на высоте 10-12 м за период между сроками',
		'Общая облачность',
		'Текущая сообщаемая погода',
		'Прошедшая погода между сроками 1',
		'Прошедшая погода между сроками 2',
		'Минимальная температура за прошедший период не более 12 часов',
		'Максимальная температура за прошедший период не более 12 часов',
		'Слоисто-кучевые, слоистые, кучевые и кучево-дождевые облака',
		'Количество всех наблюдавшихся облаков Cl или, при отсутствии таковых, Cm',
		'Высота основания самых низких облаков',
		'Высококучевые, высокослоистые и слоисто-дождевые облака',
		'Перистые, перисто-кучевые и перисто-слоистые облака',
		'Горизонтальная дальность видимости',
		'Температура точки росы на высоте 2 м над поверхностью земли',
		'Количество выпавших осадков, 0 - 0садков нет; 1е-16 - Следы осадков; Пусто - NA',
		'Период накопления осадков',
		'Состояние поверхности почвы без снега или измеримого ледяного покрова',
		'Минимальная температура поверхности почвы за ночь',
		'Состояние поверхности почвы со снегом или измеримым ледяным покровом',
		'Высота снежного покрова'),
		units = c('FLOAT',
		'FLOAT',
		'FLOAT',
		'FLOAT',
		'FLOAT',
		'INTEGER',
		'INTEGER',
		'INTEGER',
		'INTEGER',
		'INTEGER',
		'INTEGER',
		'INTEGER',
		'INTEGER',
		'INTEGER',
		'INTEGER',
		'INTEGER',
		'INTEGER',
		'FLOAT',
		'INTEGER',
		'INTEGER',
		'INTEGER',
		'FLOAT',
		'FLOAT',
		'INTEGER',
		'INTEGER',
		'FLOAT',
		'INTEGER',
		'FLOAT'
		)
)


SQL <- sqlAppendTable(conn, "rp5metadata", rp5meta, row.names = FALSE)
dbSendStatement(conn, SQL)

SQL <- c(
	"CREATE TABLE rp5levels (
		id SERIAL PRIMARY KEY,
		name VARCHAR(64),
		explanation VARCHAR(4096),
		level INTEGER
	)"
)

dbSendStatement(conn, SQL)

SQL <- c(
	"CREATE TABLE rp5data (
		id SERIAL PRIMARY KEY,
		location VARCHAR(64),
		tsp TIMESTAMP,
		T FLOAT,
		Po FLOAT,
		P FLOAT,
		Pa FLOAT,
		U FLOAT,
		DD INTEGER,
		Ff INTEGER,
		ff10 INTEGER,
		ff3 INTEGER,
		N INTEGER,
		WW INTEGER,
		W1 INTEGER,
		W2 INTEGER,
		Tn INTEGER,
		Tx INTEGER,
		Cl INTEGER,
		Nh INTEGER,
		H FLOAT,
		Cm INTEGER,
		Ch INTEGER,
		VV INTEGER,
		Td FLOAT,
		RRR FLOAT,
		tR INTEGER,
		E INTEGER,
		Tg FLOAT,
		E1 INTEGER,
		sss FLOAT
	)"
)

dbSendStatement(conn, SQL)

SQL <- c(
	"ALTER TABLE rp5data ADD FOREIGN KEY (location) REFERENCES location(locname)"
)

dbSendStatement(conn, SQL)

SQL <- c(
	"CREATE INDEX tspindex ON rp5data (tsp)"
)

dbSendStatement(conn, SQL)

SQL <- c(
	"CREATE VIEW phase_transition_time
	 as
	 SELECT genotype, inseriesnum,
		seedlings10 - sowing as sowingToSeedlings10,
		seedlings75 - sowing as sowingToSeedlings75,
		flowering10 - seedlings75 as seedlings75ToFlowering10,
		maturityFull - flowering75 as flowering75ToMaturityFull
		FROM accession"
)

dbSendStatement(conn, SQL)

SQL <- c(
	"CREATE VIEW rp5_sowing_to_seedlings10
	 as
	 SELECT genotype, inseriesnum,
		AVG(T) as T_mean_sowingToSeedlings10,
		SUM(case when T > 10 then T end) as T_sum10_sowingToSeedlings10,
		SUM(case when T > 15 then T end) as T_sum15_sowingToSeedlings10,
		SUM(T) as T_sum_sowingToSeedlings10,
		MAX(T) as T_max_sowingToSeedlings10,
		MIN(T) as T_min_sowingToSeedlings10,
		AVG(U) as U_mean_sowingToSeedlings10,
		AVG(RRR) as RRR_mean_sowingToSeedlings10,
		SUM(RRR) as RRR_sum_sowingToSeedlings10
		FROM accession, rp5data
		where
		EXTRACT(DOY FROM tsp) BETWEEN EXTRACT(DOY FROM sowing) AND EXTRACT(DOY FROM seedlings10) AND
		EXTRACT(YEAR from tsp) = EXTRACT(YEAR from sowing)
		GROUP BY genotype, inseriesnum"
)

dbSendStatement(conn, SQL)

SQL <- c(
	"CREATE VIEW rp5_sowing_to_seedlings75
	 as
	 SELECT genotype, inseriesnum,
		AVG(T) as T_mean_sowingToSeedlings75,
		SUM(case when T > 10 then T end) as T_sum10_sowingToSeedlings75,
		SUM(case when T > 15 then T end) as T_sum15_sowingToSeedlings75,
		SUM(T) as T_sum_sowingToSeedlings75,
		MAX(T) as T_max_sowingToSeedlings75,
		MIN(T) as T_min_sowingToSeedlings75,
		AVG(U) as U_mean_sowingToSeedlings75,
		AVG(RRR) as RRR_mean_sowingToSeedlings75,
		SUM(RRR) as RRR_sum_sowingToSeedlings75
		FROM accession, rp5data
		where
		EXTRACT(DOY FROM tsp) BETWEEN EXTRACT(DOY FROM sowing) AND EXTRACT(DOY FROM seedlings75) AND
		EXTRACT(YEAR from tsp) = EXTRACT(YEAR from sowing)
		GROUP BY genotype, inseriesnum"
)

#odb.write(ODB, SQL)
dbSendStatement(conn, SQL)

SQL <- c(
	"CREATE VIEW rp5_seedlings75_to_flowering10
	 as
	 SELECT genotype, inseriesnum,
		AVG(T) as T_mean_seedlings75ToFlowering10,
		SUM(case when T > 10 then T end) as T_sum10_seedlings75ToFlowering10,
		SUM(case when T > 15 then T end) as T_sum15_seedlings75ToFlowering10,
		SUM(T) as T_sum_seedlings75ToFlowering10,
		MAX(T) as T_max_seedlings75ToFlowering10,
		MIN(T) as T_min_seedlings75ToFlowering10,
		AVG(U) as U_mean_seedlings75ToFlowering10,
		AVG(RRR) as RRR_mean_seedlings75ToFlowering10,
		SUM(RRR) as RRR_sum_seedlings75ToFlowering10
		FROM accession, rp5data
		where
		EXTRACT(DOY FROM tsp) BETWEEN EXTRACT(DOY FROM seedlings75) AND EXTRACT(DOY FROM flowering10) AND
		EXTRACT(YEAR from tsp) = EXTRACT(YEAR from sowing)
		GROUP BY genotype, inseriesnum"
)

dbSendStatement(conn, SQL)

SQL <- c(
	"CREATE VIEW rp5_flowering75_to_maturityFull
	 as
	 SELECT genotype, inseriesnum,
		AVG(T) as T_mean_flowering75ToMaturityFull,
		SUM(case when T > 10 then T end) as T_sum10_flowering75ToMaturityFull,
		SUM(case when T > 15 then T end) as T_sum15_flowering75ToMaturityFull,
		SUM(T) as T_sum_flowering75ToMaturityFull,
		MAX(T) as T_max_flowering75ToMaturityFull,
		MIN(T) as T_min_flowering75ToMaturityFull,
		AVG(U) as U_mean_flowering75ToMaturityFull,
		AVG(RRR) as RRR_mean_flowering75ToMaturityFull,
		SUM(RRR) as RRR_sum_flowering75ToMaturityFull
		FROM accession, rp5data
		where
		EXTRACT(DOY FROM tsp) BETWEEN EXTRACT(DOY FROM flowering75) AND EXTRACT(DOY FROM maturityFull) AND
		EXTRACT(YEAR from tsp) = EXTRACT(YEAR from sowing)
		GROUP BY genotype, inseriesnum"
)

dbSendStatement(conn, SQL)

SQL <- c(
	"CREATE VIEW rp5_sowing
	 as
	 SELECT genotype, inseriesnum,
		AVG(T) as T_mean_sowing,
		MAX(T) as T_max_sowing,
		MIN(T) as T_min_sowing,
		AVG(U) as U_mean_sowing,
		AVG(RRR) as RRR_mean_sowing
		FROM accession, rp5data
		where
		EXTRACT(DOY FROM tsp) = EXTRACT(DOY FROM sowing) AND
		EXTRACT(YEAR from tsp) = EXTRACT(YEAR from sowing)
		GROUP BY genotype, inseriesnum"
)

dbSendStatement(conn, SQL)

SQL <- c(
	"CREATE VIEW rp5_sowing_5x5
	 as
	 SELECT genotype, inseriesnum,
		AVG(T) as T_mean_sowing_5x5,
		MAX(T) as T_max_sowing_5x5,
		MIN(T) as T_min_sowing_5x5,
		AVG(U) as U_mean_sowing_5x5,
		AVG(RRR) as RRR_mean_sowing_5x5
		FROM accession, rp5data
		where
		EXTRACT(DOY FROM tsp) BETWEEN EXTRACT(DOY FROM sowing) - 5 AND EXTRACT(DOY FROM sowing) + 5 AND
		EXTRACT(YEAR from tsp) = EXTRACT(YEAR from sowing)
		GROUP BY genotype, inseriesnum"
)

dbSendStatement(conn, SQL)

SQL <- c(
	"CREATE VIEW rp5_sowing_x10
	 as
	 SELECT genotype, inseriesnum,
		AVG(T) as T_mean_sowing_x10,
		MAX(T) as T_max_sowing_x10,
		MIN(T) as T_min_sowing_x10,
		AVG(U) as U_mean_sowing_x10,
		AVG(RRR) as RRR_mean_sowing_x10
		FROM accession, rp5data
		where
		EXTRACT(DOY FROM tsp) BETWEEN EXTRACT(DOY FROM sowing) AND EXTRACT(DOY FROM sowing) + 10 AND
		EXTRACT(YEAR from tsp) = EXTRACT(YEAR from sowing)
		GROUP BY genotype, inseriesnum"
)

dbSendStatement(conn, SQL)

SQL <- c(
	"CREATE VIEW rp5_sowing_10x
	 as
	 SELECT genotype, inseriesnum,
		AVG(T) as T_mean_sowing_10x,
		MAX(T) as T_max_sowing_10x,
		MIN(T) as T_min_sowing_10x,
		AVG(U) as U_mean_sowing_10x,
		AVG(RRR) as RRR_mean_sowing_10x
		FROM accession, rp5data
		where
		EXTRACT(DOY FROM tsp) BETWEEN EXTRACT(DOY FROM sowing) - 10 AND EXTRACT(DOY FROM sowing) AND
		EXTRACT(YEAR from tsp) = EXTRACT(YEAR from sowing)
		GROUP BY genotype, inseriesnum"
)

dbSendStatement(conn, SQL)

SQL <- c(
	"CREATE VIEW rp5_seedlings10
	 as
	 SELECT genotype, inseriesnum,
		AVG(T) as T_mean_seedlings10,
		MAX(T) as T_max_seedlings10,
		MIN(T) as T_min_seedlings10,
		AVG(U) as U_mean_seedlings10,
		AVG(RRR) as RRR_mean_seedlings10
		FROM accession, rp5data
		where
		EXTRACT(DOY FROM tsp) = EXTRACT(DOY FROM seedlings10) AND
		EXTRACT(YEAR from tsp) = EXTRACT(YEAR from seedlings10)
		GROUP BY genotype, inseriesnum"
)

dbSendStatement(conn, SQL)

SQL <- c(
	"CREATE VIEW rp5_seedlings10_5x5
	 as
	 SELECT genotype, inseriesnum,
		AVG(T) as T_mean_seedlings10_5x5,
		MAX(T) as T_max_seedlings10_5x5,
		MIN(T) as T_min_seedlings10_5x5,
		AVG(U) as U_mean_seedlings10_5x5,
		AVG(RRR) as RRR_mean_seedlings10_5x5
		FROM accession, rp5data
		where
		EXTRACT(DOY FROM tsp) BETWEEN EXTRACT(DOY FROM seedlings10) - 5 AND EXTRACT(DOY FROM seedlings10) + 5 AND
		EXTRACT(YEAR from tsp) = EXTRACT(YEAR from seedlings10)
		GROUP BY genotype, inseriesnum"
)

dbSendStatement(conn, SQL)

SQL <- c(
	"CREATE VIEW rp5_seedlings10_x10
	 as
	 SELECT genotype, inseriesnum,
		AVG(T) as T_mean_seedlings10_x10,
		MAX(T) as T_max_seedlings10_x10,
		MIN(T) as T_min_seedlings10_x10,
		AVG(U) as U_mean_seedlings10_x10,
		AVG(RRR) as RRR_mean_seedlings10_x10
		FROM accession, rp5data
		where
		EXTRACT(DOY FROM tsp) BETWEEN EXTRACT(DOY FROM seedlings10) AND EXTRACT(DOY FROM seedlings10) + 10 AND
		EXTRACT(YEAR from tsp) = EXTRACT(YEAR from seedlings10)
		GROUP BY genotype, inseriesnum"
)

dbSendStatement(conn, SQL)

SQL <- c(
	"CREATE VIEW rp5_seedlings10_10x
	 as
	 SELECT genotype, inseriesnum,
		AVG(T) as T_mean_seedlings10_10x,
		MAX(T) as T_max_seedlings10_10x,
		MIN(T) as T_min_seedlings10_10x,
		AVG(U) as U_mean_seedlings10_10x,
		AVG(RRR) as RRR_mean_seedlings10_10x
		FROM accession, rp5data
		where
		EXTRACT(DOY FROM tsp) BETWEEN EXTRACT(DOY FROM seedlings10) - 10 AND EXTRACT(DOY FROM seedlings10) AND
		EXTRACT(YEAR from tsp) = EXTRACT(YEAR from seedlings10)
		GROUP BY genotype, inseriesnum"
)

dbSendStatement(conn, SQL)

SQL <- c(
	"CREATE VIEW rp5_seedlings75
	 as
	 SELECT genotype, inseriesnum,
		AVG(T) as T_mean_seedlings75,
		MAX(T) as T_max_seedlings75,
		MIN(T) as T_min_seedlings75,
		AVG(U) as U_mean_seedlings75,
		AVG(RRR) as RRR_mean_seedlings75
		FROM accession, rp5data
		where
		EXTRACT(DOY FROM tsp) = EXTRACT(DOY FROM seedlings75) AND
		EXTRACT(YEAR from tsp) = EXTRACT(YEAR from seedlings75)
		GROUP BY genotype, inseriesnum"
)

dbSendStatement(conn, SQL)

SQL <- c(
	"CREATE VIEW rp5_seedlings75_5x5
	 as
	 SELECT genotype, inseriesnum,
		AVG(T) as T_mean_seedlings75_5x5,
		MAX(T) as T_max_seedlings75_5x5,
		MIN(T) as T_min_seedlings75_5x5,
		AVG(U) as U_mean_seedlings75_5x5,
		AVG(RRR) as RRR_mean_seedlings75_5x5
		FROM accession, rp5data
		where
		EXTRACT(DOY FROM tsp) BETWEEN EXTRACT(DOY FROM seedlings75) - 5 AND EXTRACT(DOY FROM seedlings75) + 5 AND
		EXTRACT(YEAR from tsp) = EXTRACT(YEAR from seedlings75)
		GROUP BY genotype, inseriesnum"
)

dbSendStatement(conn, SQL)

SQL <- c(
	"CREATE VIEW rp5_seedlings75_x10
	 as
	 SELECT genotype, inseriesnum,
		AVG(T) as T_mean_seedlings75_x10,
		MAX(T) as T_max_seedlings75_x10,
		MIN(T) as T_min_seedlings75_x10,
		AVG(U) as U_mean_seedlings75_x10,
		AVG(RRR) as RRR_mean_seedlings75_x10
		FROM accession, rp5data
		where
		EXTRACT(DOY FROM tsp) BETWEEN EXTRACT(DOY FROM seedlings75) AND EXTRACT(DOY FROM seedlings75) + 10 AND
		EXTRACT(YEAR from tsp) = EXTRACT(YEAR from seedlings75)
		GROUP BY genotype, inseriesnum"
)

dbSendStatement(conn, SQL)

SQL <- c(
	"CREATE VIEW rp5_seedlings75_10x
	 as
	 SELECT genotype, inseriesnum,
		AVG(T) as T_mean_seedlings75_10x,
		MAX(T) as T_max_seedlings75_10x,
		MIN(T) as T_min_seedlings75_10x,
		AVG(U) as U_mean_seedlings75_10x,
		AVG(RRR) as RRR_mean_seedlings75_10x
		FROM accession, rp5data
		where
		EXTRACT(DOY FROM tsp) BETWEEN EXTRACT(DOY FROM seedlings75) - 10 AND EXTRACT(DOY FROM seedlings75) AND
		EXTRACT(YEAR from tsp) = EXTRACT(YEAR from seedlings75)
		GROUP BY genotype, inseriesnum"
)

dbSendStatement(conn, SQL)

SQL <- c(
	"CREATE VIEW rp5_flowering10
	 as
	 SELECT genotype, inseriesnum,
		AVG(T) as T_mean_flowering10,
		MAX(T) as T_max_flowering10,
		MIN(T) as T_min_flowering10,
		AVG(U) as U_mean_flowering10,
		AVG(RRR) as RRR_mean_flowering10
		FROM accession, rp5data
		where
		EXTRACT(DOY FROM tsp) = EXTRACT(DOY FROM flowering10) AND
		EXTRACT(YEAR from tsp) = EXTRACT(YEAR from flowering10)
		GROUP BY genotype, inseriesnum"
)

dbSendStatement(conn, SQL)

SQL <- c(
	"CREATE VIEW rp5_flowering10_5x5
	 as
	 SELECT genotype, inseriesnum,
		AVG(T) as T_mean_flowering10_5x5,
		MAX(T) as T_max_flowering10_5x5,
		MIN(T) as T_min_flowering10_5x5,
		AVG(U) as U_mean_flowering10_5x5,
		AVG(RRR) as RRR_mean_flowering10_5x5
		FROM accession, rp5data
		where
		EXTRACT(DOY FROM tsp) BETWEEN EXTRACT(DOY FROM flowering10) - 5 AND EXTRACT(DOY FROM flowering10) + 5 AND
		EXTRACT(YEAR from tsp) = EXTRACT(YEAR from flowering10)
		GROUP BY genotype, inseriesnum"
)

dbSendStatement(conn, SQL)

SQL <- c(
	"CREATE VIEW rp5_flowering10_x10
	 as
	 SELECT genotype, inseriesnum,
		AVG(T) as T_mean_flowering10_x10,
		MAX(T) as T_max_flowering10_x10,
		MIN(T) as T_min_flowering10_x10,
		AVG(U) as U_mean_flowering10_x10,
		AVG(RRR) as RRR_mean_flowering10_x10
		FROM accession, rp5data
		where
		EXTRACT(DOY FROM tsp) BETWEEN EXTRACT(DOY FROM flowering10) AND EXTRACT(DOY FROM flowering10) + 10 AND
		EXTRACT(YEAR from tsp) = EXTRACT(YEAR from flowering10)
		GROUP BY genotype, inseriesnum"
)

dbSendStatement(conn, SQL)

SQL <- c(
	"CREATE VIEW rp5_flowering10_10x
	 as
	 SELECT genotype, inseriesnum,
		AVG(T) as T_mean_flowering10_10x,
		MAX(T) as T_max_flowering10_10x,
		MIN(T) as T_min_flowering10_10x,
		AVG(U) as U_mean_flowering10_10x,
		AVG(RRR) as RRR_mean_flowering10_10x
		FROM accession, rp5data
		where
		EXTRACT(DOY FROM tsp) BETWEEN EXTRACT(DOY FROM flowering10) - 10 AND EXTRACT(DOY FROM flowering10) AND
		EXTRACT(YEAR from tsp) = EXTRACT(YEAR from flowering10)
		GROUP BY genotype, inseriesnum"
)

dbSendStatement(conn, SQL)

SQL <- c(
	"CREATE VIEW rp5_maturityFull
	 as
	 SELECT genotype, inseriesnum,
		AVG(T) as T_mean_maturityFull,
		MAX(T) as T_max_maturityFull,
		MIN(T) as T_min_maturityFull,
		AVG(U) as U_mean_maturityFull,
		AVG(RRR) as RRR_mean_maturityFull
		FROM accession, rp5data
		where
		EXTRACT(DOY FROM tsp) = EXTRACT(DOY FROM maturityFull) AND
		EXTRACT(YEAR from tsp) = EXTRACT(YEAR from maturityFull)
		GROUP BY genotype, inseriesnum"
)

dbSendStatement(conn, SQL)

SQL <- c(
	"CREATE VIEW rp5_maturityFull_5x5
	 as
	 SELECT genotype, inseriesnum,
		AVG(T) as T_mean_maturityFull_5x5,
		MAX(T) as T_max_maturityFull_5x5,
		MIN(T) as T_min_maturityFull_5x5,
		AVG(U) as U_mean_maturityFull_5x5,
		AVG(RRR) as RRR_mean_maturityFull_5x5
		FROM accession, rp5data
		where
		EXTRACT(DOY FROM tsp) BETWEEN EXTRACT(DOY FROM maturityFull) - 5 AND EXTRACT(DOY FROM maturityFull) + 5 AND
		EXTRACT(YEAR from tsp) = EXTRACT(YEAR from maturityFull)
		GROUP BY genotype, inseriesnum"
)

dbSendStatement(conn, SQL)

SQL <- c(
	"CREATE VIEW rp5_maturityFull_x10
	 as
	 SELECT genotype, inseriesnum,
		AVG(T) as T_mean_maturityFull_x10,
		MAX(T) as T_max_maturityFull_x10,
		MIN(T) as T_min_maturityFull_x10,
		AVG(U) as U_mean_maturityFull_x10,
		AVG(RRR) as RRR_mean_maturityFull_x10
		FROM accession, rp5data
		where
		EXTRACT(DOY FROM tsp) BETWEEN EXTRACT(DOY FROM maturityFull) AND EXTRACT(DOY FROM maturityFull) + 10 AND
		EXTRACT(YEAR from tsp) = EXTRACT(YEAR from maturityFull)
		GROUP BY genotype, inseriesnum"
)

dbSendStatement(conn, SQL)

SQL <- c(
	"CREATE VIEW rp5_maturityFull_10x
	 as
	 SELECT genotype, inseriesnum,
		AVG(T) as T_mean_maturityFull_10x,
		MAX(T) as T_max_maturityFull_10x,
		MIN(T) as T_min_maturityFull_10x,
		AVG(U) as U_mean_maturityFull_10x,
		AVG(RRR) as RRR_mean_maturityFull_10x
		FROM accession, rp5data
		where
		EXTRACT(DOY FROM tsp) BETWEEN EXTRACT(DOY FROM maturityFull) - 10 AND EXTRACT(DOY FROM maturityFull) AND
		EXTRACT(YEAR from tsp) = EXTRACT(YEAR from maturityFull)
		GROUP BY genotype, inseriesnum"
)

dbSendStatement(conn, SQL)

SQL <- c(
	"CREATE VIEW clim_pheno
	 as
	 SELECT phase_transition_time.genotype, phase_transition_time.inseriesnum,
		sowingToSeedlings10,
		sowingToSeedlings75,
		seedlings75ToFlowering10,
		flowering75ToMaturityFull,
		T_mean_sowingToSeedlings10,
		T_sum10_sowingToSeedlings10,
		T_sum15_sowingToSeedlings10,
		T_sum_sowingToSeedlings10,
		T_max_sowingToSeedlings10,
		T_min_sowingToSeedlings10,
		U_mean_sowingToSeedlings10,
		RRR_mean_sowingToSeedlings10,
		RRR_sum_sowingToSeedlings10,
		T_mean_sowingToSeedlings75,
		T_sum10_sowingToSeedlings75,
		T_sum15_sowingToSeedlings75,
		T_sum_sowingToSeedlings75,
		T_max_sowingToSeedlings75,
		T_min_sowingToSeedlings75,
		U_mean_sowingToSeedlings75,
		RRR_mean_sowingToSeedlings75,
		RRR_sum_sowingToSeedlings75,
		T_mean_seedlings75ToFlowering10,
		T_sum10_seedlings75ToFlowering10,
		T_sum15_seedlings75ToFlowering10,
		T_sum_seedlings75ToFlowering10,
		T_max_seedlings75ToFlowering10,
		T_min_seedlings75ToFlowering10,
		U_mean_seedlings75ToFlowering10,
		RRR_mean_seedlings75ToFlowering10,
		RRR_sum_seedlings75ToFlowering10,
		T_mean_flowering75ToMaturityFull,
		T_sum10_flowering75ToMaturityFull,
		T_sum15_flowering75ToMaturityFull,
		T_sum_flowering75ToMaturityFull,
		T_max_flowering75ToMaturityFull,
		T_min_flowering75ToMaturityFull,
		U_mean_flowering75ToMaturityFull,
		RRR_mean_flowering75ToMaturityFull,
		RRR_sum_flowering75ToMaturityFull,
		T_mean_sowing,
		T_max_sowing,
		T_min_sowing,
		U_mean_sowing,
		RRR_mean_sowing,
		T_mean_sowing_5x5,
		T_max_sowing_5x5,
		T_min_sowing_5x5,
		U_mean_sowing_5x5,
		RRR_mean_sowing_5x5,
		T_mean_sowing_x10,
		T_max_sowing_x10,
		T_min_sowing_x10,
		U_mean_sowing_x10,
		RRR_mean_sowing_x10,
		T_mean_sowing_10x,
		T_max_sowing_10x,
		T_min_sowing_10x,
		U_mean_sowing_10x,
		RRR_mean_sowing_10x,
		T_mean_seedlings10,
		T_max_seedlings10,
		T_min_seedlings10,
		U_mean_seedlings10,
		RRR_mean_seedlings10,
		T_mean_seedlings10_5x5,
		T_max_seedlings10_5x5,
		T_min_seedlings10_5x5,
		U_mean_seedlings10_5x5,
		RRR_mean_seedlings10_5x5,
		T_mean_seedlings10_x10,
		T_max_seedlings10_x10,
		T_min_seedlings10_x10,
		U_mean_seedlings10_x10,
		RRR_mean_seedlings10_x10,
		T_mean_seedlings10_10x,
		T_max_seedlings10_10x,
		T_min_seedlings10_10x,
		U_mean_seedlings10_10x,
		RRR_mean_seedlings10_10x,
		T_mean_seedlings75,
		T_max_seedlings75,
		T_min_seedlings75,
		U_mean_seedlings75,
		RRR_mean_seedlings75,
		T_mean_seedlings75_5x5,
		T_max_seedlings75_5x5,
		T_min_seedlings75_5x5,
		U_mean_seedlings75_5x5,
		RRR_mean_seedlings75_5x5,
		T_mean_seedlings75_x10,
		T_max_seedlings75_x10,
		T_min_seedlings75_x10,
		U_mean_seedlings75_x10,
		RRR_mean_seedlings75_x10,
		T_mean_seedlings75_10x,
		T_max_seedlings75_10x,
		T_min_seedlings75_10x,
		U_mean_seedlings75_10x,
		RRR_mean_seedlings75_10x,
		T_mean_flowering10,
		T_max_flowering10,
		T_min_flowering10,
		U_mean_flowering10,
		RRR_mean_flowering10,
		T_mean_flowering10_5x5,
		T_max_flowering10_5x5,
		T_min_flowering10_5x5,
		U_mean_flowering10_5x5,
		RRR_mean_flowering10_5x5,
		T_mean_flowering10_x10,
		T_max_flowering10_x10,
		T_min_flowering10_x10,
		U_mean_flowering10_x10,
		RRR_mean_flowering10_x10,
		T_mean_flowering10_10x,
		T_max_flowering10_10x,
		T_min_flowering10_10x,
		U_mean_flowering10_10x,
		RRR_mean_flowering10_10x,
		T_mean_maturityFull,
		T_max_maturityFull,
		T_min_maturityFull,
		U_mean_maturityFull,
		RRR_mean_maturityFull,
		T_mean_maturityFull_5x5,
		T_max_maturityFull_5x5,
		T_min_maturityFull_5x5,
		U_mean_maturityFull_5x5,
		RRR_mean_maturityFull_5x5,
		T_mean_maturityFull_x10,
		T_max_maturityFull_x10,
		T_min_maturityFull_x10,
		U_mean_maturityFull_x10,
		RRR_mean_maturityFull_x10,
		T_mean_maturityFull_10x,
		T_max_maturityFull_10x,
		T_min_maturityFull_10x,
		U_mean_maturityFull_10x,
		RRR_mean_maturityFull_10x
	FROM phase_transition_time,
		rp5_sowing_to_seedlings10,
		rp5_sowing_to_seedlings75,
		rp5_seedlings75_to_flowering10,
		rp5_flowering75_to_maturityFull,
		rp5_sowing,
		rp5_sowing_5x5,
		rp5_sowing_x10,
		rp5_sowing_10x,
		rp5_seedlings10,
		rp5_seedlings10_5x5,
		rp5_seedlings10_x10,
		rp5_seedlings10_10x,
		rp5_seedlings75,
		rp5_seedlings75_5x5,
		rp5_seedlings75_x10,
		rp5_seedlings75_10x,
		rp5_flowering10,
		rp5_flowering10_5x5,
		rp5_flowering10_x10,
		rp5_flowering10_10x,
		rp5_maturityFull,
		rp5_maturityFull_5x5,
		rp5_maturityFull_x10,
		rp5_maturityFull_10x
	WHERE
		phase_transition_time.genotype = rp5_sowing_to_seedlings10.genotype AND
		phase_transition_time.inseriesnum = rp5_sowing_to_seedlings10.inseriesnum AND
		phase_transition_time.genotype = rp5_sowing_to_seedlings75.genotype AND
		phase_transition_time.inseriesnum = rp5_sowing_to_seedlings75.inseriesnum AND
		phase_transition_time.genotype = rp5_seedlings75_to_flowering10.genotype AND
		phase_transition_time.inseriesnum = rp5_seedlings75_to_flowering10.inseriesnum AND
		phase_transition_time.genotype = rp5_flowering75_to_maturityFull.genotype AND
		phase_transition_time.inseriesnum = rp5_flowering75_to_maturityFull.inseriesnum AND
		phase_transition_time.genotype = rp5_sowing.genotype AND
		phase_transition_time.inseriesnum = rp5_sowing.inseriesnum AND
		phase_transition_time.genotype = rp5_sowing_5x5.genotype AND
		phase_transition_time.inseriesnum = rp5_sowing_5x5.inseriesnum AND
		phase_transition_time.genotype = rp5_sowing_x10.genotype AND
		phase_transition_time.inseriesnum = rp5_sowing_x10.inseriesnum AND
		phase_transition_time.genotype = rp5_sowing_10x.genotype AND
		phase_transition_time.inseriesnum = rp5_sowing_10x.inseriesnum AND
		phase_transition_time.genotype = rp5_seedlings10.genotype AND
		phase_transition_time.inseriesnum = rp5_seedlings10.inseriesnum AND
		phase_transition_time.genotype = rp5_seedlings10_5x5.genotype AND
		phase_transition_time.inseriesnum = rp5_seedlings10_5x5.inseriesnum AND
		phase_transition_time.genotype = rp5_seedlings10_x10.genotype AND
		phase_transition_time.inseriesnum = rp5_seedlings10_x10.inseriesnum AND
		phase_transition_time.genotype = rp5_seedlings10_10x.genotype AND
		phase_transition_time.inseriesnum = rp5_seedlings10_10x.inseriesnum AND
		phase_transition_time.genotype = rp5_seedlings75.genotype AND
		phase_transition_time.inseriesnum = rp5_seedlings75.inseriesnum AND
		phase_transition_time.genotype = rp5_seedlings75_5x5.genotype AND
		phase_transition_time.inseriesnum = rp5_seedlings75_5x5.inseriesnum AND
		phase_transition_time.genotype = rp5_seedlings75_x10.genotype AND
		phase_transition_time.inseriesnum = rp5_seedlings75_x10.inseriesnum AND
		phase_transition_time.genotype = rp5_seedlings75_10x.genotype AND
		phase_transition_time.inseriesnum = rp5_seedlings75_10x.inseriesnum AND
		phase_transition_time.genotype = rp5_flowering10.genotype AND
		phase_transition_time.inseriesnum = rp5_flowering10.inseriesnum AND
		phase_transition_time.genotype = rp5_flowering10_5x5.genotype AND
		phase_transition_time.inseriesnum = rp5_flowering10_5x5.inseriesnum AND
		phase_transition_time.genotype = rp5_flowering10_x10.genotype AND
		phase_transition_time.inseriesnum = rp5_flowering10_x10.inseriesnum AND
		phase_transition_time.genotype = rp5_flowering10_10x.genotype AND
		phase_transition_time.inseriesnum = rp5_flowering10_10x.inseriesnum AND
		phase_transition_time.genotype = rp5_maturityFull.genotype AND
		phase_transition_time.inseriesnum = rp5_maturityFull.inseriesnum AND
		phase_transition_time.genotype = rp5_maturityFull_5x5.genotype AND
		phase_transition_time.inseriesnum = rp5_maturityFull_5x5.inseriesnum AND
		phase_transition_time.genotype = rp5_maturityFull_x10.genotype AND
		phase_transition_time.inseriesnum = rp5_maturityFull_x10.inseriesnum AND
		phase_transition_time.genotype = rp5_maturityFull_10x.genotype AND
		phase_transition_time.inseriesnum = rp5_maturityFull_10x.inseriesnum"
)

dbSendStatement(conn, SQL)
