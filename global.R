library(readxl); library(data.table); library(magrittr)

# readxl
data17 <- read_excel("datafile.xlsx", sheet=1)
data18 <- read_excel("datafile.xlsx", sheet=2)
data19 <- read_excel("datafile.xlsx", sheet=3)

varname <- c("age", "sex", "polyp_location", "gross_size", "specimen_size", "paris_class", "histology", "polyp_no", "withdrawal_t", "intubation_t", "piecemeal_resec", "margin", "adjuctive_apc", "clip", "immediate_bleed", "delayed_bleed", "perforation", "postpolypectomy_sdr", "admission")
varname2 <- c("prepR1", "prepT1", "prepL1", "prepS1", "prepR2", "prepT2", "prepL2", "prepS2", "interval")

data19 <- data19 %>% as.data.table() %>% .[,c(7:25, 36:39, 41:44, 46)]
names(data19)[1:19] <- varname
names(data19)[20:28] <- varname2

data18 <- data18 %>% as.data.table() %>% .[,c(7:25, 37:40, 42:45, 47)]
names(data18)[1:19] <- varname
names(data18)[20:28] <- varname2

data17 <- data17 %>% as.data.table() %>% .[,c(7:23)]
names(data17) <- c("age", "sex", "polyp_location", "gross_size", "specimen_size", "paris_class", "histology", "polyp_no", "piecemeal_resec", "margin", "adjuctive_apc", "clip", "immediate_bleed", "delayed_bleed", "perforation", "postpolypectomy_sdr", "admission")

# dt1
dt1 <- rbind(data18[,year:=2018], data19[,year:=2019]) %>% .[,c(1:19,29)]

vars.cont <- c("specimen_size", "withdrawal_t", "intubation_t")
dt1[, (vars.cont) := lapply(.SD, as.numeric), .SDcols = vars.cont]

dt1 <- merge(dt1, data17[,year:=2017], all=T)
dt1[,'histology_cancer' := ifelse(histology %in% 4:5, 1, 0)]

vars.conti1 <- c("age", "gross_size", "polyp_no", vars.cont)
vars.cat1 <- setdiff(c(varname, "year", "histology_cancer"), vars.conti1)
dt1[, (vars.cat1) := lapply(.SD, as.factor), .SDcols = vars.cat1]

# dt2
dt2 <- rbind(data18[,year:=2018], data19[,year:=2019]) %>% .[,c(20:29)] %>% na.omit()
vars.prep <-c("prepR", "prepT", "prepL", "prepS")
for (vn in vars.prep){
  vn1 <- paste0(vn, 1)
  vn2 <- paste0(vn, 2)
  vndiff <- paste0(vn, "diff")
  dt2[, vndiff] <- dt2[, ..vn1]-dt2[, ..vn2]
}
vars.cat2 <- c(paste0(vars.prep, "diff", recycle0=T), "year")
vars.conti2 <- setdiff(names(dt2), vars.cat2)
dt2[, (vars.cat2) := lapply(.SD, as.factor), .SDcols = vars.cat2]
