### RR Optional Ploting Code

## Section 1 : Download and Read Data
## Create Folder if not exists

if(!file.exists("./RR_Optional_Plotting")){
  
  dir.create("./RR_Optional_Plotting")
  
}

##Download file if not exists
if(!file.exists("./RR_Optional_Plotting/payments.csv")){
  

fileUrl <- "https://d3c33hcgiwev3.cloudfront.net/_e143dff6e844c7af8da2a4e71d7c054d_payments.csv?Expires=1507507200&Signature=Bo8hlzhdXawzPNafHxEEsFDESbfI2dSjfuDBpDIpLu2N9cVgMaOHb01nr9OhqtwbWmtudF3oU9tvU-jN9IY9RQSU0NB7vbBAIIc3VzshqzcPlHrsk0ejlgbL2lOuJPDCiMgcWuaaBLzmGcxZoL4748W7TkZXzxfJqKs5NdI4zAY_&Key-Pair-Id=APKAJLTNE6QMUY6HBC5A"
download.file(fileUrl, destfile = "./RR_Optional_Plotting/payments.csv")
}
payments <- read.csv("./RR_Optional_Plotting/payments.csv")


## Section 2 : Create Plot 1
par(mfrow=c(1,2))

with(subset(payments,Provider.State=="NY"),
     plot(Average.Covered.Charges,Average.Total.Payments,
          xlab = "Average Covered Charges", 
          ylab = "Average Total Payments", 
          pch=19,
          col="red"))

abline(lm(Average.Total.Payments~Average.Covered.Charges, subset(payments,Provider.State=="NY")),
     lwd = 2,col="black")
## Add Main title
mtext("Mean Covered Charges - Mean Total Payments(Relationship in New York)", side = 3, line = 2, cex = 1.4, adj=-0.5,col = "Black")



## Second instance of plot 1 in log scale
with(subset(payments,Provider.State=="NY"),
     plot(Average.Covered.Charges,Average.Total.Payments,
          xlab = "Average Covered Charges", 
          ylab = "Average Total Payments", 
          log = "xy",
          pch=19,
          col="red"))
legend("topleft", "logscale")

abline(lm(Average.Total.Payments~Average.Covered.Charges, subset(payments,Provider.State=="NY")),
       lwd = 2,col="black",untf = TRUE)




###Export Plot 1 into a pdf file
dev.copy2pdf(file = "./RR_Optional_Plotting/plot1.pdf",width = 20, height = 12,paper="a4r" )

dev.off()

##Section 3 : Create Plot 2

## Customise figures shown per row
plot2rows<-nlevels(payments$Provider.State)/3
par(mfrow=c(plot2rows,3))

## Customise color palette and how many colors are needed
plot2colors<-nlevels(payments$DRG.Definition
                     )

##Choose colors from RColorBrewer package Dark2 set
library(RColorBrewer)
DRGcolors <- brewer.pal(plot2colors, "Dark2")




## Make colors a transparent
DRGcolorstran <- paste(DRGcolors, sprintf("%x", ceiling(255*0.5)), sep = "")



##Creating Figures

for (i in levels(payments$Provider.State)) {
  statedata<-subset(payments,Provider.State==i)
  ##DRGcol<-DRGcolorstran[DRG.Definition]
  with(statedata,
       plot(Average.Covered.Charges,Average.Total.Payments,
            pch=19,
            log = "xy",
            xlab = "", 
            ylab = "",
            col=DRGcolorstran[DRG.Definition]
       ))
  legend("topleft", "logscale")
  legend("bottomright",i)
  Linecol=1
  for (j in levels(statedata$DRG.Definition)){
    
    abline(lm(Average.Total.Payments~Average.Covered.Charges, subset(statedata, statedata$DRG.Definition==j)),
           lwd = 2,col=DRGcolorstran[Linecol],untf = TRUE)
    Linecol=Linecol+1
  }
}
## Add main title
mtext("Mean Covered Charges(X.axis) Mean Total Payments(Y.axis) by Medical Condition and by State ", side = 3, line = 32, cex = 1.3,adj=1.1,col = "Black")

## Add Color Legends
par(fig = c(0, 1, 0, 1), oma = c(0, 0, 0, 0), mar = c(0, 4, 0, 0), new = TRUE)

plot(0, 0, type = "n", bty = "n", xaxt = "n", yaxt = "n")

legend("bottom", legend = levels(payments$DRG.Definition), xpd = TRUE, 
       
       inset = c(0, 0), bty = "n", pch = 19, col = DRGcolors, ncol = 2,
       
       y.intersp = 0.8, cex = 0.7)

## Write ouput to pdf file
dev.copy2pdf(file = "./RR_Optional_Plotting/plot2.pdf",width=24,height=12,paper="a4r" )

dev.off()