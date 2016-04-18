PDM<-function(){
cat("WELCOME TO PHOTOGRAMETRIC DISTANCES MEASURE")
library(jpeg)
#Select JPG
image<-file.choose()
#read JPG
specimen<-readJPEG(image)
#Open JPG
windows(6,6); plot(seq(0, dim(specimen)[2], length.out = 10), seq(0, dim(specimen)[1], length.out = 10), type = "n", xlab = "x", ylab = "y", asp = 1, tck = 0, xaxt = "n", yaxt = "n")
rasterImage(specimen, 1, 1, dim(specimen)[2], dim(specimen)[1])
#Ask scale in mm
readline(prompt="Enter scale value en mm = ")->scale_mm
as.numeric(scale_mm)->scale_mm
#Select start and end points of scale
windows(6,6, title = "Select start and end points for scale"); plot(seq(0, dim(specimen)[2], length.out = 10), seq(0, dim(specimen)[1], length.out = 10), type = "n", xlab = "x", ylab = "y", asp = 1, tck = 0, xaxt = "n", yaxt = "n")
rasterImage(specimen, 1, 1, dim(specimen)[2], dim(specimen)[1])
xy_scale<<-locator(n=2, type="l")
#Calcule mm for px
output<-as.matrix(xy_scale$x)
output<-cbind(output,as.matrix(xy_scale$y))
dist_output<-dist(output)
number_dist<-as.numeric(dist_output)
scale_mm/number_dist->mm_pixel
#Create measures table
measures<-data.frame()
#i= TRUE for flow control
i<-TRUE
while(i==TRUE){
#Ask for name of measure
readline(prompt="Enter name of measure = ")->name_LM
windows(6,6, title = "Select start and end points for measure"); plot(seq(0, dim(specimen)[2], length.out = 10), seq(0, dim(specimen)[1], length.out = 10), type = "n", xlab = "x", ylab = "y", asp = 1, tck = 0, xaxt = "n", yaxt = "n")
rasterImage(specimen, 1, 1, dim(specimen)[2], dim(specimen)[1])
xy_LM<<-locator(n=2, type="l")
#calcule dist in px of measure
output<-as.matrix(xy_LM$x)
output<-cbind(output,as.matrix(xy_LM$y))
dist_output<-dist(output)
dist_LM<-as.numeric(dist_output)
#calcule dist LM in mm
LM_mm<-dist_LM*mm_pixel
#show in console the measure
output_measure<-paste("measure",name_LM,"=>", LM_mm, "mm")
output_measure

#Include measure in matrix measures
measures_LM<-cbind(name_LM, LM_mm)
measures<-rbind.data.frame(measures, measures_LM)

#ask for a new measure
cat(output_measure)
cat("\n Do you want to do other measure?")
switch(menu(c("yes", "no")), yes = i<-TRUE, no = i<-FALSE)
}
return(measures)
}