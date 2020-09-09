############################################################
### Make rosetta-table (Cohens D->raw diff) for article  ###
############################################################
## Pontus P. Sigray, August 2020 

source('./R/Tables/Rosetta_table/source/convert_effsize_rawdata.R')
source('./R/Tables/Rosetta_table/source/fetch_mean.R')

D.grid <- c(0.2, 0.5, 0.8, 1.2, 1.5)
Raclopride <- convert_effsize_rawdata(measures = D.grid,type_in = 'D',type_out = 'raw',radioligand = 'Raclopride_HRRT')
Raclopride_perc <- (Raclopride/fetch_mean('Raclopride_HRRT')) * 100

AZ9369 <- convert_effsize_rawdata(measures = D.grid,type_in = 'D',type_out = 'raw',radioligand = 'AZ9369_HRRT')
AZ9369_perc <- (AZ9369/fetch_mean('AZ9369_HRRT')) * 100

PBR28 <- convert_effsize_rawdata(measures = D.grid,type_in = 'D',type_out = 'raw',radioligand = 'PBR28_HRRT')
PBR28_perc <- (PBR28/fetch_mean('PBR28_HRRT')) * 100

UCBJ <- convert_effsize_rawdata(measures = D.grid,type_in = 'D',type_out = 'raw',radioligand = 'UCBJ_HRRT')
UCBJ_perc <- (UCBJ/fetch_mean('UCBJ_HRRT')) * 100


Cross.sec.rosetta <- data.frame(D_or_Dz = 'D',
                                Effsize = D.grid,
                                raclo_BPND = paste0( round(Raclopride,2),' (', round(Raclopride_perc),"\\%)" ),
                                AZ_BPND = paste0( round(AZ9369,2),' (', round(AZ9369_perc),"\\%)" ),
                                PBR28_VT = paste0( round(PBR28,2),' (', round(PBR28_perc),"\\%)" ),
                                UCBJ_VT = paste0( round(UCBJ,2),' (', round(UCBJ_perc),"\\%)" ))


Dz.grid <- c(0.2, 0.5, 0.8, 1.2, 1.5)
Raclopride <- convert_effsize_rawdata(measures = Dz.grid,type_in = 'Dz',type_out = 'raw',radioligand = 'Raclopride_HRRT')
Raclopride_perc <- (Raclopride/fetch_mean('Raclopride_HRRT')) * 100

AZ9369 <- convert_effsize_rawdata(measures = Dz.grid,type_in = 'Dz',type_out = 'raw',radioligand = 'AZ9369_HRRT')
AZ9369_perc <- (AZ9369/fetch_mean('AZ9369_HRRT')) * 100

PBR28 <- convert_effsize_rawdata(measures = Dz.grid,type_in = 'Dz',type_out = 'raw',radioligand = 'PBR28_HRRT')
PBR28_perc <- (PBR28/fetch_mean('PBR28_HRRT')) * 100

UCBJ <- convert_effsize_rawdata(measures = Dz.grid,type_in = 'Dz',type_out = 'raw',radioligand = 'UCBJ_HRRT')
UCBJ_perc <- (UCBJ/fetch_mean('UCBJ_HRRT')) * 100


Long.rosetta <- data.frame(D_or_Dz = 'Dz',
                                Effsize = Dz.grid,
                                raclo_BPND = paste0( round(Raclopride,2),' (', round(Raclopride_perc),"\\%)" ),
                                AZ_BPND = paste0( round(AZ9369,2),' (', round(AZ9369_perc),"\\%)" ),
                                PBR28_VT = paste0( round(PBR28,2),' (', round(PBR28_perc),"\\%)" ),
                                UCBJ_VT = paste0( round(UCBJ,2),' (', round(UCBJ_perc),"\\%)" ))


Rosetta <- rbind(Cross.sec.rosetta,Long.rosetta)

xlsx::write.xlsx(x = Rosetta,file = './Results/Tables/Rosetta_table_R.xlsx',row.names = F)






