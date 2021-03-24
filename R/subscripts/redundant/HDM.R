# HDM
# Lewis A. Jones
# March 2020
#---------------------------------
library(biomod2)
library(dismo)
library(raster)
library(viridis)
library(rJava)
#---------------------------------

data <- read.csv("./data/occurrences/UNEP_pts_subsample.csv")

head(data)

env <- stack(list.files(path="./data/enm/layers/Modern/", pattern='asc', full.names=TRUE))
#plot(env)

ext <- extract(x = env, y = data[,c("x", "y")], df = TRUE)
data <- cbind.data.frame(data, ext)
data <- na.omit(data)
data <- data[,c("species", "x", "y")]
colnames(data)[1] <- "reef"
data$reef <- 1
#---------------------------------
# load the environmental raster layers (could be .img, ArcGIS 
# rasters or any supported format by the raster package)

myBiomodData <- BIOMOD_FormatingData(resp.var = data[,c("reef")], 
                                     expl.var = env, 
                                     resp.xy = data[,c("x", "y")], 
                                     resp.name = "reef",
                                     PA.nb.absences = 10000,
                                     PA.nb.rep = 1, #define how many random background point samples you want
                                     PA.strategy = 'random',
                                     na.rm = TRUE)

background_data <- cbind.data.frame(myBiomodData@coord, myBiomodData@PA)
write.csv(background_data, "./data/enm/background_data.csv")

#============< DEFINE MODEL OPTIONS >=====================
myBiomodOption <- BIOMOD_ModelingOptions( 
  MAXENT.Phillips = list(path_to_maxent.jar="E:\\maxent.jar/",
                         maximumiterations = 5000, 
                         visible = FALSE, 
                         linear = TRUE,
                         quadratic = TRUE, 
                         product = FALSE, 
                         threshold = FALSE, 
                         hinge = FALSE, 
                         lq2lqptthreshold = 70,
                         l2lqthreshold = 10, 
                         hingethreshold = 15, 
                         beta_threshold = -1, 
                         beta_categorical = -1, 
                         beta_lqp = -1, 
                         beta_hinge = -1, 
                         defaultprevalence = 0.5,
                         betamultiplier=1)) ### numeric (default 1), multiply all automatic regularization parameters by this number. A higher number gives a more spread-out distribution.


#============< COMPUTE MODEL >=====================

myBiomodModelOut <- BIOMOD_Modeling(myBiomodData, 
                                    models =  'MAXENT.Phillips',
                                    models.options = myBiomodOption, 
                                    NbRunEval=1, 
                                    DataSplit=80, 
                                    Yweights=NULL, 
                                    VarImport=5, 
                                    models.eval.meth = c('TSS','ROC','KAPPA', 'FAR',
                                                         'SR', 'ACCURACY', 'BIAS', 'POD', 'CSI', 'ETS'), 
                                    SaveObj = TRUE, 
                                    rescal.all.models = FALSE,
                                    do.full.models = FALSE,
                                    modeling.id = "Reefs",
                                    tanspose = FALSE)

#============< RESPONSE PLOT >================================

myMaxent <- BIOMOD_LoadModels(myBiomodModelOut, models='MAXENT.Phillips')

png("./figures/response_plot1.png", width = 150, height = 150, units = "mm", res = 300)

response.plot <- response.plot2(models = myMaxent,
                                Data = get_formal_data(myBiomodModelOut, 'expl.var'),
                                show.variables = get_formal_data(myBiomodModelOut, 'expl.var.names'),
                                do.bivariate = FALSE,
                                fixed.var.metric = 'median',
                                col = viridis(begin = 0, end = 0.9, 10),
                                ylab = "Suitability",
                                legend = FALSE,
                                display_title = FALSE,
                                data_species = get_formal_data(myBiomodModelOut,'resp.var'))

dev.off()

png("./figures/response_plot2.png", width = 150, height = 150, units = "mm", res = 300)
par(mfrow=c(2,2))
for(i in 1:length(response.plot)){
  tmp <- response.plot[[i]]
  var <- tmp[,1]
  name <- colnames(tmp[1])
  mean <- rowMeans(tmp[,2:101])
  plot(mean ~ var, pch = 20, type = "l", lwd = 3, col = "#bd0026", lty = 1, ylab = "Suitability", xlab = name)
}

dev.off()

for(i in 1:100){
  rm(list = (paste("z.coral_PA1_RUN", i, "_MAXENT.Phillips", sep = "")))
}

#============< LOOK AT MODEL EVALUATION >=====================

myBiomodModelEval <- get_evaluations(myBiomodModelOut)
eval <- as.data.frame(myBiomodModelEval)
write.csv(eval, "./outputs/evaluation_scores/model_eval.csv", row.names = FALSE)
var.import <- as.data.frame(get_variables_importance(myBiomodModelOut))
write.csv(var.import, "./outputs/var_import/var_import.csv", row.names = FALSE)

#write data
auc_table<-NULL
TSS_table<-NULL
kappa_table<-NULL
far_table<-NULL
sr_table<-NULL
accuracy_table<-NULL
bias_table<-NULL
pod_table<-NULL
csi_table<-NULL
ets_table<-NULL

ROC<-myBiomodModelEval["ROC","Testing.data",,,]
TSS<-myBiomodModelEval["TSS","Testing.data",,,]
KAPPA<-myBiomodModelEval["KAPPA","Testing.data",,,]
FAR<-myBiomodModelEval["FAR","Testing.data",,,]
SR<-myBiomodModelEval["SR","Testing.data",,,]
ACCURACY<-myBiomodModelEval["ACCURACY","Testing.data",,,]
BIAS<-myBiomodModelEval["BIAS","Testing.data",,,]
POD<-myBiomodModelEval["POD","Testing.data",,,]
CSI<-myBiomodModelEval["CSI","Testing.data",,,]
ETS<-myBiomodModelEval["ETS","Testing.data",,,]
#bind tables
auc_table<-cbind(auc_table,ROC)
TSS_table<-cbind(TSS_table,TSS)
kappa_table<-cbind(kappa_table,KAPPA)
far_table<-cbind(far_table,FAR)
sr_table<-cbind(sr_table,SR)
accuracy_table<-cbind(accuracy_table,ACCURACY)
bias_table<-cbind(bias_table,BIAS)
pod_table<-cbind(pod_table,POD)
csi_table<-cbind(csi_table,CSI)
ets_table<-cbind(ets_table,ETS)

cmb<- cbind(TSS_table, auc_table, kappa_table, far_table, sr_table, accuracy_table, bias_table, pod_table,csi_table, ets_table)
#write results
write.csv(cmb, "./outputs/evaluation_scores/cmb_results.csv", row.names = FALSE)

#============< MODERN MODEL PROJECTIONS >================================

Modern <- BIOMOD_Projection(
  modeling.output = myBiomodModelOut,
  new.env = env,
  proj.name = 'modern',
  selected.models = 'all',
  binary.meth = c('TSS', 'ROC'),
  compress = 'xz',
  build.clamping.mask = TRUE,
  do.stack = TRUE,
  output.format='.grd',
  on_0_1000 = FALSE)

PredModern <- get_predictions(Modern)
plot(PredModern$z.coral_PA1_RUN1_MAXENT.Phillips)

dir.create("./outputs/asc/Pre-Industrial_280_real/")
writeRaster(PredModern, file="./outputs/asc/Pre-Industrial_280_real/", filename=names(PredModern), bylayer = TRUE, format="ascii", overwrite=TRUE)
mean <- mean(PredModern)
writeRaster(mean, file="./outputs/asc/Pre-Industrial_280_real/mean_prediction.asc", format="ascii", overwrite=TRUE)

TSS <- raster(paste("./z.coral/proj_modern/proj_modern_z.coral_TSSbin.grd", sep = ""))
writeRaster(TSS, file=paste("./outputs/asc/Pre-Industrial_280_real/TSS_prediction.asc", sep = ""), format="ascii", overwrite=TRUE)

LPT.Threshold <- raster::extract(x = mean, y = cbind.data.frame(modern_occ$x, modern_occ$y))
LPT.Threshold <- sort(LPT.Threshold)
LPT.Threshold <- na.omit(LPT.Threshold)
#LPT.Threshold <- LPT.Threshold[round(length(LPT.Threshold)*0.05)]
LPT.Threshold <- LPT.Threshold[1]
LPT <- BinaryTransformation(mean, LPT.Threshold)
plot(LPT)
writeRaster(LPT, file=paste("./outputs/asc/Pre-Industrial_280_real/LPT_prediction.asc", sep = ""), format="ascii", overwrite=TRUE)

#============< PHANEROZOIC MODEL PROJECTIONS >================================

stages <- list.files("F:\\OneDrive - Imperial College London/Coral_LBG_paper/Data/formatted_data/prepared/")
stages <- stages[!stages == "Pre-Industrial_280_real"]

for(i in stages){
  
  layers <- stack(list.files(path=paste("F:\\OneDrive - Imperial College London/Coral_LBG_paper/Data/formatted_data/prepared/", i, "/", sep =""),pattern='asc',full.names=TRUE))
  
  # 5. Project our models over reducation scenarios 
  deeptime <- BIOMOD_Projection(
    modeling.output = myBiomodModelOut,
    new.env = layers,
    proj.name = i,
    selected.models = 'all',
    binary.meth = c('TSS', 'ROC'),
    compress = 'xz',
    clamping.mask = TRUE,
    do.stack = TRUE,
    output.format='.grd',
    on_0_1000 = FALSE)
  
  Preddeeptime <- get_predictions(deeptime)
  mask <- raster(paste("./z.coral/proj_",i, "/proj_", i, "_ClampingMask.grd", sep = ""))
  mask[mask > 0] <- 1
  Preddeeptime <- mask(x = Preddeeptime, mask = mask, maskvalue = 1, updatevalue = 0)
  plot(Preddeeptime$z.coral_PA1_RUN1_MAXENT.Phillips)
  dir.create(paste("./outputs/asc/", i, "/", sep = ""))
  writeRaster(Preddeeptime, file=paste("./outputs/asc/", i, "/", sep = ""), filename=names(Preddeeptime), bylayer = TRUE, format="ascii", overwrite=TRUE)
  mean <- mean(Preddeeptime)
  plot(mean)
  writeRaster(mean, file=paste("./outputs/asc/", i, "/mean_prediction.asc", sep = ""), format="ascii", overwrite=TRUE)
  
  #binary rasters
  
  TSS <- raster(paste("./z.coral/proj_", i, "/proj_", i, "_z.coral_TSSbin.grd", sep = ""))
  TSS <- mask(x = TSS, mask = mask, maskvalue = 1, updatevalue = 0)
  writeRaster(TSS, file=paste("./outputs/asc/", i, "/TSS_prediction.asc", sep = ""), format="ascii", overwrite=TRUE)
  
  LPT <- BinaryTransformation(mean, LPT.Threshold)
  LPT <- mask(x = LPT, mask = mask, maskvalue = 1, updatevalue = 0)
  writeRaster(LPT, file=paste("./outputs/asc/", i, "/LPT_prediction.asc", sep = ""), format="ascii", overwrite=TRUE)
}
