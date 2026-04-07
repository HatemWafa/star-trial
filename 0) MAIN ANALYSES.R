


# IMPORT DATASET -----------------------------------------------------------------------------
ds <- read.csv("anonymous_star_20221111.csv")
# ============================================================================================





# LIBRARIES ----------------------------------------------------------------------------------
library(foreign)
library(xtable)
library(tableone)
library(reshape2)
library(readxl)
library(magrittr)
# ============================================================================================





# DEFINED FUNCTIONS --------------------------------------------------------------------------
source("tab1_lancet.R")
source("LinReg.R")
source("lineplot.R")
source("stair.R")
source("parameters.R")
# ============================================================================================





# TABLE 1: BASELINE CHARACTERISTICS -----------------------------------------------------------
vars = c(
  # Demography:
  "age", "sex", "race", "rfsmok_0",
  # Ophthalmic history:
  "duramd", "npavt_se", "se_lenss_0", "se_valet_0", "FA_TotalLesionArea_0", "FA_TotalActiveLesionArea_0", "se_octthi_0", "OCT_TotalMacularVolume_0",
  # QOL
  "vfq_comp_0", "eq5tot_0",
  "eq5mob_0", "eq5car_0", "eq5act_0", "eq5pain_0", "eq5anx_0"
)

# Sex & Race & Smoking status
ds$sex      <- ds$sex %>% factor(levels=c(1,2), labels=c("Male","Female"))
ds$race     <- ds$race %>% factor(levels=c(1:6), labels=c("White", "Black", "Asian", "Other", "Other", "Other"))
ds$rfsmok_0 <- ds$rfsmok_0 %>% factor(levels=c(1,2,3), labels=c("Current smoker", "Ex-smoker", "Non-smoker"))

nonnormal = c("duramd", "npavt_se", "FA_TotalLesionArea_0", "FA_TotalActiveLesionArea_0",
              grep("^vfq|eq5",vars, value = T)
)


TBL1 <- tab1(
  # mtable = TRUE,
  # mcodes = NA,
  vars = vars,
  strata = "arm",
  data = ds[!is.na(ds$arm),],
  nonnormal = nonnormal,
  overall = TRUE,
  test = FALSE,
  contDigits = 1,
  decichar = "·",
  lancet=TRUE
) %>% .[,c(2,1,3)]
# ============================================================================================





# TABLE 2: PRIMARY & SECONDARY EFFICACY OUTCOMES----------------------------------------------
for(m in c(12,24,36,48)) {
  
  ds[,paste0("se_valet_",m)] <- ds[,paste0("se_valet_",m,"m")] %>% as.numeric
  # ds[,paste0("se_valet_",m)] %>% hist
  
  ds[,paste0("vax_",m)]   <- ds[,paste0("vax_",m)] %>% as.numeric                              #Change in VA
  ds[,paste0("val15_",m)] <- ifelse(ds[,paste0("vax_",m)] > -15, 1, 0)  %>% as.factor          #Losing < 15 ETDRS letters
  ds[,paste0("vag15_",m)] <- ifelse(ds[,paste0("vax_",m)] >= 15, 1, 0)  %>% as.factor          #Gaining ≥ 15 ETDRS letters
  ds[,paste0("vag0_",m)] <- ifelse(ds[,paste0("vax_",m)] >= 0, 1, 0)    %>% as.factor          #Gaining ≥ 0 ETDRS letters 
  
  ds[,paste0("se_gld_",m)] <- ds[,paste0("se_gld_",m)] %>% as.numeric
  ds[,paste0("se_gld_",m)] <- ds[,paste0("se_gld_",m)] * 1000
  ds[,paste0("se_gld_",m)] %>% summary
  # ds[,paste0("se_gld_",m)] %>% hist
  
  ds[,paste0("se_octthi_",m)] <- ds[,paste0("se_octthi_",m,"m")] %>% as.numeric
  ds[,paste0("se_octthi_",m)] %>% summary
  # ds[,paste0("se_octthi_",m)] %>% hist
  
  ds[,paste0("FA_TotalLesionArea_m",m)] <- ds[,paste0("FA_TotalLesionArea_m",m)] %>% as.numeric
  ds[,paste0("FA_TotalLesionArea_m",m)] %>% summary
  # ds[,paste0("FA_TotalLesionArea_m",m)] %>% hist
  
  ds[,paste0("FA_TotalActiveLesionArea_m",m)] <- ds[,paste0("FA_TotalActiveLesionArea_m",m)] %>% as.numeric
  ds[,paste0("FA_TotalActiveLesionArea_m",m)] %>% summary
  # ds[,paste0("FA_TotalActiveLesionArea_m",m)] %>% hist
  
  ds[,paste0("vfqx_",m)] <- (ds[,paste0("vfq_comp_",m,"m")] - ds$vfq_comp_0) %>% as.numeric         #Change in VA
}



vars = sapply(
  c(
    "ninjadm_", 
    "vax_",
    "se_valet_",
    "val15_",
    "vag0_",
    "vag15_",
    "FA_TotalLesionArea_m",
    "FA_TotalActiveLesionArea_m",
    "se_octthi_",
    "vfq_comp_",
    "eq5tot_"
  ),
  function(x) paste0(x,c(12,24,36,48))
) %>% as.vector
vars[grep("^vfq_comp|eq5tot",vars)] <- paste0(grep("^vfq_comp|eq5tot",vars,value=TRUE),"m")

vars= vars[vars%in%names(ds)]
nonnormal = grep("^vfq_comp|eq5tot|FA_TotalLesionArea|FA_TotalActiveLesionArea",vars,value=TRUE)

TBL2 <- tab1(
  # mtable = TRUE,
  # mcodes = NA,
  vars = vars,
  strata = "arm",
  data = ds[!is.na(ds$arm),],
  overall = TRUE,
  test = FALSE,
  nonnormal = nonnormal,
  contDigits = 1,
  lancet=TRUE
) %>% .[,c(2,1,3)] %>% as.data.frame

TBL2$Regression <- rep("",nrow(TBL2))
TBL2$Regression[2] <- LinReg("ninjadm_12", c("arm", "recsite"), ds, 1) %>% .[2,] %>% paste(collapse = ", ")
TBL2$Regression[3] <- LinReg("ninjadm_24", c("arm", "recsite"), ds, 1) %>% .[2,] %>% paste(collapse = ", ")
TBL2$Regression[4] <- LinReg("ninjadm_36", c("arm", "recsite"), ds, 1) %>% .[2,] %>% paste(collapse = ", ")
TBL2$Regression[5] <- LinReg("ninjadm_48", c("arm", "recsite"), ds, 1) %>% .[2,] %>% paste(collapse = ", ")

TBL2$Regression[6] <- LinReg("vax_12", c("arm", "recsite", "se_valet_0"), ds, 1) %>% .[2,] %>% paste(collapse = ", ")
TBL2$Regression[7] <- LinReg("vax_24", c("arm", "recsite", "se_valet_0"), ds, 1) %>% .[2,] %>% paste(collapse = ", ")
TBL2$Regression[8] <- LinReg("vax_36", c("arm", "recsite", "se_valet_0"), ds, 1) %>% .[2,] %>% paste(collapse = ", ")
TBL2$Regression[9] <- LinReg("vax_48", c("arm", "recsite", "se_valet_0"), ds, 1) %>% .[2,] %>% paste(collapse = ", ")


TBL2 <- TBL2 %>% apply(c(1,2), function(x) {
  # x <- x %>% round_numbers(uto=1, lto=1)
  x <- gsub("\\.", "·", x)
  x <- gsub(", <", ", p<", x)
  x <- gsub("\\*", "", x)
  # x <- gsub(" - ", ' to ', x)
})
# ============================================================================================





# AVERAGE NUMBER OF INJECTIONS RECEIVED ------------------------------------------------------
ds$ninjj_36 <- ds$ninjadm_36
ds$ninjj_48 <- ds$ninjadm_48
ds[,paste0("ninjj_",c(25:35,37:47))] <- NA

# Plot measures:
y <- data.frame(
  "Sham" = c(apply(ds[ds$arm=="Sham",paste0("ninjj_",c(1:48))],2,mean,na.rm=TRUE) %>% as.vector),
  "SRT"  = c(apply(ds[ds$arm=="SRT",paste0("ninjj_",c(1:48))],2,mean,na.rm=TRUE) %>% as.vector)
)
# Standard error to plot error bars:
stderror <- data.frame(
  "se1" = c(apply(ds[ds$arm=="Sham", paste0("ninjj_",c(1:48))], 2, function(x) {sd(x, na.rm=TRUE) / (((x %>% na.omit %>% length)-1) %>% sqrt)})),
  "se2" = c(apply(ds[ds$arm=="SRT", paste0("ninjj_",c(1:48))], 2, function(x) {sd(x, na.rm=TRUE) / (((x %>% na.omit %>% length)-1) %>% sqrt)}))
)

lineplot(
  x=1:48, y=y,
  # oma = c(2,2,2.5,0),
  stderror = stderror,
  errorbars = "bars",
  type=c("p","p"),
  # type=c("o","o"),
  lty=c(1,1), 
  lwd = c(0.6,0.6),
  pch=c(15,15),
  cols = c("blue", "red"),
  xtitle = "Visit week",
  ytitle = "Cumulative number of Anti-VEGF injections",
  xticklab = FALSE,
  yticklab = FALSE,
  ylimit = c(0,25),
  leg.lab = c("Sham SRT", "SRT"),
  leg.pos = "topleft",
  cex=1,
  cex.pt=c(0.8,0.8)
)

axis(1, at=c(0:24,36,48), labels = NA, tcl=-0.2, col=NA, col.ticks=1)
axis(1, at=seq(0,10,1), labels = paste0("",seq(0,10,1)*4), tcl=0,col=NA, col.ticks=1, mgp=c(1,0.2,0.03))
axis(1, at=c(12,24,36,48), labels = c(12,24,36,48)*4, font=2, tcl=0,col=NA, col.ticks=1, mgp=c(1,0.2,0.03))

axis(2, at=seq(0,24,2), labels = NA, tcl=-0.2, col=NA, col.ticks=1)
axis(2, at=seq(0,24,2), labels = seq(0,24,2), tcl=0,col=NA, col.ticks=1, las=1)
# ============================================================================================





# BCVA CHANGE OVER TIME ----------------------------------------------------------------------
# Plot measures:
y <- data.frame(
  "Sham" = c(0, apply(ds[ds$arm=="Sham",paste0("vax_",1:48)],2,mean,na.rm=TRUE) %>% as.vector, NA),
  "SRT"  = c(0, apply(ds[ds$arm=="SRT",paste0("vax_",1:48)],2,mean,na.rm=TRUE) %>% as.vector, NA)
)
# Standard error to plot error bars:
stderror <- data.frame(
  "se1" = c(NA, apply(ds[ds$arm=="Sham", paste0("vax_",1:48)], 2, function(x) {sd(x, na.rm=TRUE) / (((x %>% na.omit %>% length)-1) %>% sqrt)}),NA),
  "se2" = c(NA, apply(ds[ds$arm=="SRT", paste0("vax_",1:48)], 2, function(x) {sd(x, na.rm=TRUE) / (((x %>% na.omit %>% length)-1) %>% sqrt)}),NA)
)

lineplot(
  x=0:49, y=y,
  stderror = stderror,
  errorbars = "bars",
  type=c("p","p"),
  lty=c(1,1), 
  pch=c(15,15),
  cols = c("blue", "red"),
  xtitle = "Visit week",
  ytitle = "Change in visual acuity from baseline (letter score)",
  xticklab = FALSE, yticklab = FALSE,
  ylimit = c(-20,10),
  leg.lab = c("Sham SRT", "SRT"),
  leg.pos = "topleft",
  cex=1,
  cex.pt = c(0.8,0.8)
)
abline(h=0, lty=2, col="grey")

axis(2, at=seq(-20,5), labels = seq(-20,5), tcl=-0.2, col=NA, col.ticks=1)

Arrows(x0=49.5, y0=-1, x1=49.5, y1=-10, arr.type = "triangle", arr.length = 0.2, arr.width = 0.15)
lines(x=c(49.5,49.5), y=c(-0.3,-1), lty=2)
text(x=50.5, y=(-10--0.3)/2, labels="Worse vision", srt=90, cex=0.8)

Arrows(x0=49.5, y0=1, x1=49.5, y1=10, arr.type = "triangle", arr.length = 0.2, arr.width = 0.15)
lines(x=c(49.5,49.5), y=c(0.3,1), lty=2)
text(x=50.5, y=(10-0.3)/2, labels="Better vision", srt=90, cex=0.8)

axis(1, at=c(0:24,36,48), labels = NA, tcl=-0.2, col=NA, col.ticks=1)
axis(1, at=seq(0,10,1), labels = paste0("",seq(0,10,1)*4), tcl=0,col=NA, col.ticks=1, mgp=c(1,0.2,0.03))
axis(1, at=c(12,24,36,48), labels = c(12,24,36,48)*4, font=2, tcl=0,col=NA, col.ticks=1, mgp=c(1,0.2,0.03))
# ============================================================================================





# CST CHANGE OVER TIME -----------------------------------------------------------------------
# Plot measures:
y <- data.frame(
  "Sham" = c(0, apply(ds[ds$arm=="Sham",paste0("octx_",1:48)],2,mean,na.rm=TRUE) %>% as.vector, NA),
  "SRT"  = c(0, apply(ds[ds$arm=="SRT",paste0("octx_",1:48)],2,mean,na.rm=TRUE) %>% as.vector, NA)
)
# Standard error to plot error bars:
stderror <- data.frame(
  "se1" = c(NA, apply(ds[ds$arm=="Sham", paste0("octx_",1:48)], 2, function(x) {sd(x, na.rm=TRUE) / (((x %>% na.omit %>% length)-1) %>% sqrt)}), NA),
  "se2" = c(NA, apply(ds[ds$arm=="SRT", paste0("octx_",1:48)], 2, function(x) {sd(x, na.rm=TRUE) / (((x %>% na.omit %>% length)-1) %>% sqrt)}), NA)
)

lineplot(
  x=0:49, y=y,
  stderror = stderror,
  errorbars = "bars",
  type=c("p","p"),
  lty=c(1,1), 
  pch=c(15,15),
  cols = c("blue", "red"),
  xtitle = "Visit week",
  ytitle = "Change in central subfield thickness from baseline (microns)",
  xticklab = FALSE, yticklab = FALSE,
  ylimit = c(-120,60),
  leg.lab = c("Sham SRT", "SRT"),
  leg.pos = "topleft",
  cex=1,
  cex.pt=c(0.8,0.8)
)
abline(h=0, lty=2, col="grey")

axis(1, at=c(0:24,36,48), labels = NA, tcl=-0.2, col=NA, col.ticks=1)
axis(1, at=seq(0,10,1), labels = paste0("",seq(0,10,1)*4), tcl=0,col=NA, col.ticks=1, mgp=c(1,0.2,0.03))
axis(1, at=c(12,24,36,48), labels = c(12,24,36,48)*4, font=2, tcl=0,col=NA, col.ticks=1, mgp=c(1,0.2,0.03))

axis(2, at=seq(-120,60,20), labels = seq(-120,60,20), tcl=-0.2, col=NA, col.ticks=1)
# ============================================================================================





# COMPLIANCE PLOT ----------------------------------------------------------------------------
# which scheduled visits were during covid lockdown (23 March 2020)
ds$dtcov <- as.Date("2020-03-23") %>% as.numeric
ds$ffucov <- NA
for(sv in c(1:24,36,48)) {
  ds$dtsched <- ds$dtvis_0m + (28*sv)
  ds$ffucov <- ifelse(ds$dtsched>=ds$dtcov & is.na(ds$ffucov),sv,ds$ffucov)
  ds[,paste0("covis_",sv)] <- ifelse(ds$dtsched>=ds$dtcov,1,0)
}

# Prepare data in long format
ds_long1 <- gather(
  ds[,c("id", "arm",paste0("rag_",c(1:24,36,48)))],
  visit,
  RAG,
  rag_1:rag_48
)
ds_long2 <- gather(
  ds[,c("id",paste0("covis_",c(1:24,36,48)))],
  visit,
  covis,
  covis_1:covis_48
)

ds_long1$visit <- gsub("rag_","", ds_long1$visit) %>% as.numeric
ds_long2$visit <- gsub("covis_","", ds_long2$visit) %>% as.numeric

dsl <- merge(x=ds_long1, y=ds_long2, by=c("id","visit"))
dsl <- dsl[,c("id","visit","covis","RAG", "arm")]
rm(ds_long1, ds_long2)


dta <- ds[,c(
  "id", "dtvis_0m", "ffucov", 
  paste0("rag_",c(1:24,36,48)), 
  paste0("dtvis_",c(1:24,36,48),"m"),
  paste0("covis_", c(1:24,36,48))
)]
# dta <- dta[order(dta$ffucov, decreasing = TRUE, na.last = FALSE),]
dta <- dta[order(dta$dtvis_0m, decreasing = FALSE, na.last = FALSE),]
rownames(dta) <- 1:411

x=dta$ffucov
x[x==36] <- 25.0
x[x==48] <- 26.0
# x[x==36] <- NA
# x[x==48] <- NA
y=1:length(x)


## PLOT ACTUAL FIGURE
par(family="serif")
par(oma = c(3,3,0.5,0),
    mar = c(0.5,0,0,0),
    mgp = c(1,0.5,0.03),
    cex = 0.9,
    cex.axis = 0.8,
    cex.main = 0.8,
    lwd = 0.8
)

plot(x=NA, y=NA, type="s", bty="n",
     xlab=NA, ylab=NA,
     yaxs="i", xaxs="i",
     yaxt="n", xaxt="n",
     # xlim=c(1,28),
     xlim=c(1,26.0+2),
     ylim=c(0,420+50)
)

col=c("#ec7176","#f4ab32","#b3e4d0","white","grey99", "#1c4b76")

for(i in 1:411) {
  c <- dta[i,paste0("rag_",c(1:24,36,48))] %>% unlist %>%  as.vector
  c <- sapply(c, function(x) col[x])
  for(v in c(1:24,25.0,26.0)) {
    polygon(x=c(v,v+1,v+1,v),
            y=c(i,i,i-1,i-1),
            col = c[v],
            border = NA
    )
  }
}


# x-axis
axis(1, at=c(1:24,27), labels = NA, tcl=-0.2, col=NA, col.ticks=1)
axis(1, at=c(1,(25-0.2)), labels = NA, tcl=0) #, col=NA, col.ticks=1)
axis(1, at=c((25+0.2),(26-0.2)), labels = NA, tcl=0) #, col=NA, col.ticks=1)
axis(1, at=c((26+0.2),(27-0.0)), labels = NA, tcl=0) #, col=NA, col.ticks=1)



par(xpd = NA) # to allow plotting outside (overlay the margins)
segments(25-0.2, -0.4, 25.1-0.2, -0.4, col = "black", lwd = 1)
segments(25.1-0.2, -0.4, 25.15-0.2, 7, col = "black", lwd = 1)
segments(25.15-0.2, 7, 25.2-0.2, -7, col = "black", lwd = 1)
segments(25.2-0.2, -7, 25.25-0.2, -0.4, col = "black", lwd = 1)
segments(25.25-0.2, -0.4, 25.4-0.2, -0.4, col = "black", lwd = 1)

segments(26-0.2, -0.4, 26.1-0.2, -0.4, col = "black", lwd = 1)
segments(26.1-0.2, -0.4, 26.15-0.2, 7, col = "black", lwd = 1)
segments(26.15-0.2, 7, 26.2-0.2, -7, col = "black", lwd = 1)
segments(26.2-0.2, -7, 26.25-0.2, -0.4, col = "black", lwd = 1)
segments(26.25-0.2, -0.4, 26.4-0.2, -0.4, col = "black", lwd = 1)
par(xpd = FALSE)


axis(1, at=c(1)+0.5, labels = paste0("",c(1*4)), tcl=0.0, col=NA, col.ticks=1)
axis(1, at=c(2:23,25.0,26.0)+0.5, labels = paste0("",c(2:23,36,48)*4), tcl=0.0, col=NA, col.ticks=1)

axis(1, at=(24)+0.5, labels = 24*4,  tcl=0.0, col=NA, col.ticks=1, font = 2)

mtext("Visit week", side=1, line=1.3, outer=TRUE, las=1, cex=1, font=1, at=0.44)

# y-axis
anntick <- table(dta$ffucov, useNA = "ifany")
anntick <- c(
  anntick[length(anntick)] %>% sum,
  anntick[c(which(names(anntick) %in% c("48")), length(anntick))] %>% sum,
  anntick[c(which(names(anntick) %in% c("36","48")), length(anntick))] %>% sum,
  anntick[c(which(names(anntick) %in% as.character(c(13:24,36,48))), length(anntick))] %>% sum
)
axis(2, at=c(1,anntick,411)-0.5, labels = NA, las=2, tcl=-0.2, mgp = c(1,0.5,0.5))#, col=NA, col.ticks=1)
axis(2, at=c(1,anntick,411)-0.5, labels = c(1,anntick,411), las=3, tcl=-0.0, col=NA, col.ticks=1, mgp = c(1,1,0.5))

mtext("Participant number", side=2, line=2, outer=TRUE, las=3, cex=1, font=1)



# COVID line
xy <- data.frame("x"=x, "y"=y)
xy <- xy[!is.na(xy$x),]
lines(x=c(xy$x[1]+1, xy$x), y=c(xy$y[1], xy$y), type = "s", col="blue")

lines(x=c(4,4), y=c(411,415), col="blue")
text(x=4, y=419, labels ="[COVID-19 lockdown]", cex=0.9, col="blue")

# LEGEND
legend(x= 1, y=470,
       legend = c("Poor compliance", "Withdrawal"),
       bty="n",
       fill = c("#ec7176", "white")
)

legend(x= 8, y=470,
       legend = c("Reduced compliance", "Death"),
       bty="n",
       fill = c("#f4ab32", "#1c4b76")
)

legend(x= 16, y=470,
       legend = c("Per protocol"),
       bty="n",
       fill = c("#b3e4d0")
)
# ============================================================================================





# LTF CHARACTERISTICS ------------------------------------------------------------------------
## BMJ reviewer request
ds$miss_36 <- ds$ninjadm_36 %>% is.na
ds$miss_48 <- ds$ninjadm_48 %>% is.na

vars = c(
  # Demography:
  "age", "sex", "race", "rfsmok_0",
  # Ophthalmic history:
  "duramd", "npavt_se", "se_lenss_0", "se_valet_0", "FA_TotalLesionArea_0", "FA_TotalActiveLesionArea_0", "se_octthi_0", "OCT_TotalMacularVolume_0",
  # QOL
  "vfq_comp_0", "eq5tot_0",
  "eq5mob_0", "eq5car_0", "eq5act_0", "eq5pain_0", "eq5anx_0"
)
nonnormal = c("duramd", "npavt_se", "FA_TotalLesionArea_0", "FA_TotalActiveLesionArea_0",
              grep("^vfq|eq5",vars, value = T)
)

R1 <- cbind(
  tab1(
    # mtable = TRUE,
    # mcodes = NA,
    vars = vars,
    strata = "miss_36",
    data = ds[!is.na(ds$arm),],
    nonnormal = nonnormal,
    overall = FALSE,
    test = FALSE,
    contDigits = 1,
    decichar = "·",
    lancet=TRUE
  ), #%>% .[,c(2,1)] #%>% apply(c(1,2), round_numbers(uto=1,lto=1))
  
  
  tab1(
    # mtable = TRUE,
    # mcodes = NA,
    vars = vars,
    strata = "miss_48",
    data = ds[!is.na(ds$arm),],
    nonnormal = nonnormal,
    overall = FALSE,
    test = FALSE,
    contDigits = 1,
    decichar = "·",
    lancet=TRUE
  ) #%>% .[,c(2,1)] #%>% apply(c(1,2), round_numbers(uto=1,lto=1))
)
# ============================================================================================





# BASELINE CHARACTERISTICS BY SEX ------------------------------------------------------------
## BMJ reviewer request
R2 <- cbind(
  tab1(
    vars = vars,
    strata = "arm",
    data = ds[!is.na(ds$arm) & ds$sex=="Male",],
    nonnormal = nonnormal,
    overall = FALSE,
    test = FALSE,
    contDigits = 1,
    decichar = "·",
    lancet=TRUE
  ) %>% .[,c(2,1)], #%>% apply(c(1,2), round_numbers(uto=1,lto=1))
  
  tab1(
    vars = vars,
    strata = "arm",
    data = ds[!is.na(ds$arm) & ds$sex=="Female",],
    nonnormal = nonnormal,
    overall = FALSE,
    test = FALSE,
    contDigits = 1,
    decichar = "·",
    lancet=TRUE
  ) %>% .[,c(2,1)] #%>% apply(c(1,2), round_numbers(uto=1,lto=1))
)
# ============================================================================================





# MISSINGNESS --------------------------------------------------------------------------------
vars = sapply(
  c(
    "ninjadm_", 
    "vax_",
    "se_valet_",
    "val15_",
    "vag0_",
    "vag15_",
    "FA_TotalLesionArea_m",
    "FA_TotalActiveLesionArea_m",
    "se_octthi_",
    "vfq_comp_",
    "eq5tot_"
  ),
  function(x) paste0(x,c(12,24,36,48))
) %>% as.vector
vars[grep("^vfq_comp|eq5tot",vars)] <- paste0(grep("^vfq_comp|eq5tot",vars,value=TRUE),"m")

vars <- c(
  grep("12$|_12m$",vars,value=T)[c(-2,-4,-5,-6)],
  grep("24$|_24m$",vars,value=T)[c(-2,-4,-5,-6)],
  grep("36$|_36m$",vars,value=T)[c(-2,-4,-5,-6)],
  grep("48$|_48m$",vars,value=T)[c(-2,-4,-5,-6)]
)

vars <- c(
  c(
    # Demography:
    "age", "sex", "race", "rfsmok_0",
    # Ophthalmic history:
    "duramd", "npavt_se", "se_lenss_0", "se_valet_0", "FA_TotalLesionArea_0", "FA_TotalActiveLesionArea_0", "se_octthi_0", "OCT_TotalMacularVolume_0",
    # QOL
    "vfq_comp_0", "eq5tot_0",
    "eq5mob_0", "eq5car_0", "eq5act_0", "eq5pain_0", "eq5anx_0"
  ),
  vars
)

TBL <- tab1(
  mtable = TRUE,
  mcodes = c(NA),
  vars = vars,
  strata = "arm",
  data = ds[!is.na(ds$arm),],
  overall = TRUE,
  test = FALSE,
  contDigits = 1,
  decichar = "·",
  lancet = TRUE
) %>% .[,c(2,1,3)] #%>% apply(c(1,2), round_numbers, lto=2, uto=2)

S1 <- TBL
# ============================================================================================





# SUBGROUPS: MVA | FOVEA ---------------------------------------------------------------------
TBL <- rbind(
  # MVA POSITIVE
  tab1(vars = "vax_12", strata = "arm", data = ds[ds$mva_12=="Yes" & !is.na(ds$mva_12),], overall = TRUE, test = FALSE),
  tab1(vars = "vax_24", strata = "arm", data = ds[ds$mva_24=="Yes" & !is.na(ds$mva_24),], overall = TRUE, test = FALSE),
  tab1(vars = "vax_36", strata = "arm", data = ds[ds$mva_36=="Yes" & !is.na(ds$mva_36),], overall = TRUE, test = FALSE),
  tab1(vars = "vax_48", strata = "arm", data = ds[ds$mva_48=="Yes" & !is.na(ds$mva_48),], overall = TRUE, test = FALSE),
  
  # MVA NEGATIVE
  tab1(vars = "vax_12", strata = "arm", data = ds[ds$mva_12=="No" & !is.na(ds$mva_12),], overall = TRUE, test = FALSE),
  tab1(vars = "vax_24", strata = "arm", data = ds[ds$mva_24=="No" & !is.na(ds$mva_24),], overall = TRUE, test = FALSE),
  tab1(vars = "vax_36", strata = "arm", data = ds[ds$mva_36=="No" & !is.na(ds$mva_36),], overall = TRUE, test = FALSE),
  tab1(vars = "vax_48", strata = "arm", data = ds[ds$mva_48=="No" & !is.na(ds$mva_48),], overall = TRUE, test = FALSE),
  
  # FOVEA-INVOLVING MVA
  tab1(vars = "vax_12", strata = "arm", data = ds[ds$mva_invol_12=="YES" & !is.na(ds$mva_invol_12),], overall = TRUE, test = FALSE),
  tab1(vars = "vax_24", strata = "arm", data = ds[ds$mva_invol_24=="YES" & !is.na(ds$mva_invol_24),], overall = TRUE, test = FALSE),
  tab1(vars = "vax_36", strata = "arm", data = ds[ds$mva_invol_36=="YES" & !is.na(ds$mva_invol_36),], overall = TRUE, test = FALSE),
  tab1(vars = "vax_48", strata = "arm", data = ds[ds$mva_invol_48=="YES" & !is.na(ds$mva_invol_48),], overall = TRUE, test = FALSE),
  
  # NON-FOVEA-INVOLVING MVA
  tab1(vars = "vax_12", strata = "arm", data = ds[ds$mva_invol_12=="NO" & !is.na(ds$mva_invol_12),], overall = TRUE, test = FALSE),
  tab1(vars = "vax_24", strata = "arm", data = ds[ds$mva_invol_24=="NO" & !is.na(ds$mva_invol_24),], overall = TRUE, test = FALSE),
  tab1(vars = "vax_36", strata = "arm", data = ds[ds$mva_invol_36=="NO" & !is.na(ds$mva_invol_36),], overall = TRUE, test = FALSE),
  tab1(vars = "vax_48", strata = "arm", data = ds[ds$mva_invol_48=="NO" & !is.na(ds$mva_invol_48),], overall = TRUE, test = FALSE)
  
) %>% as.data.frame %>% .[,c(2,1,3)] %>% apply(c(1,2), round_numbers)

S2 <- cbind(
  TBL[c(1:nrow(TBL))[c(1:nrow(TBL)) %% 2 != 0],] %>% apply(c(1,2), Numextract),
  TBL[c(1:nrow(TBL))[c(1:nrow(TBL)) %% 2 == 0],]
) %>% apply(c(1,2), function(x) gsub("\\.","·",x))

rownames(S2) <- 
  sapply(c("mva_pos_", "mva_neg_", "fovea_in_", "non_fovea_in_"), function(x) paste0(x, c(12,24,36,48))) %>% as.vector
# ============================================================================================





# DISTRIBUTION OF INJECTIONS RECEIVED --------------------------------------------------------
# Plot measures:
ds$dist48 <- ds$ninjadm_48 %>% factor(levels = 1:48, labels = 1:48)
y <- table(ds$dist48, ds$arm) %>% prop.table(margin = 2) %>% as.matrix.data.frame %>% as.data.frame
y <- y * 100
names(y) <- c("Sham SRT", "SRT")

y <- y %>% apply(2, stair, direction = "y")
x <- c(1:48) 
x  <- x %>% stair(step = x[2]-x[1], direction = "x")



lineplot(
  x=cbind(x-0.03,x+0.03), y=y,
  # stderror = stderror,
  type=c("l","l"),
  lty=c(1,1), 
  # pch=c(20,20),
  cols = c("blue", "red"),
  xtitle = "Number of administered anti-VEGF injections over 192 weeks",
  ytitle = "Percentage of participant",
  xticklab = FALSE, 
  yticklab = TRUE,
  # ylimit = c(0,16),
  leg.lab = c("Sham SRT", "SRT"),
  cex=0.9,
  leg.pos="topleft"
)
axis(1, at=1:48, labels = 1:48, tcl=-0.2, col=NA, col.ticks=1, mgp=c(1,0.2,0.03))

x1 <- x-0.03
polygon(x = c(x1, x1[length(x1)], x1[1]),                           
        y = c(y[,1], 0, 0),                          
        col = "blue" %>% makeTransparent(alpha = 0.1), 
        border = NA)

x2 <- x+0.03
polygon(x = c(x2, x2[length(x2)], x2[1]),                           
        y = c(y[,2], 0, 0),                          
        col = "red" %>% makeTransparent(alpha = 0.1), 
        border = NA)
# ============================================================================================





# XXXXXXXXXXXXXXXXXXXXXXXXX ------------------------------------------------------------------

# ============================================================================================





# XXXXXXXXXXXXXXXXXXXXXXXXX ------------------------------------------------------------------

# ============================================================================================





# XXXXXXXXXXXXXXXXXXXXXXXXX ------------------------------------------------------------------

# ============================================================================================





# XXXXXXXXXXXXXXXXXXXXXXXXX ------------------------------------------------------------------

# ============================================================================================
