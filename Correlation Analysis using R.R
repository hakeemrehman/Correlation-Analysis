library(ggplot2)

# Read/Load the data: '.csv file' (comma-separated values file)
examData = read.csv(file.choose(),header=TRUE)

examData = read.delim(file.choose(),header=TRUE)

# Read/Load the data (Method-2)
examData <- read.delim("Exam Anxiety.dat",  header = TRUE)
names(examData)

View(examData)

#--------Scatterplots----------
#Simple scatter
scatter <- ggplot(examData, aes(Anxiety, Exam))
scatter + geom_point() + labs(x = "Exam Anxiety", y = "Exam Performance %") 

#Grouped (Gender) scatter
scatter <- ggplot(examData, aes(Anxiety, Exam, colour = Gender))
scatter + geom_point() + labs(x = "Exam Anxiety", y = "Exam Performance %", colour = "Gender") 

# ------------Parametric Correlation Analysis-------------

#The assumption of normality
#---------------------------

# Histogram
hist.Exam_Anxiety <- hist(examData$Anxiety)
hist.Exam_Anxiety

hist.Exam_Performance <- hist(examData$Exam)
hist.Exam_Performance

# Q-Q Plot
qqplot.Exam_Anxiety <- qplot(sample = examData$Anxiety)
qqplot.Exam_Anxiety

qqplot.Exam_Performance <- qplot(sample = examData$Exam)
qqplot.Exam_Performance

library(psych)
describe(examData$Exam)

library(pastecs)
stat.desc(examData$Exam, basic = FALSE, norm = TRUE)


# More than One Variable
describe(examData[,c("Anxiety", "Exam")])

# options(scipen=999) for removing the scientific notation
stat.desc(examData[,c("Anxiety", "Exam")], basic = FALSE, norm = TRUE)

# Shapiro–Wilk test
shapiro.test(examData$Exam)
shapiro.test(examData$Anxiety)

# Pearson Correlation Coefficient
#--------------------------------
cor(examData$Exam, examData$Anxiety, method = 'pearson')
examData2 <- examData[, c("Exam", "Anxiety", "Revise")]
cor(examData2)

# Coefficient of Determination
cor(examData2)^2 

'cor.test(x, y,alternative = c("two.sided", "less", "greater"),
      method = c("pearson", "kendall", "spearman"), conf.level = 0.95'
cor.test(examData$Exam, examData$Anxiety, alternative = "less", 
         method ="pearson", conf.level = 0.99)

# -----Correlation Analysis Using Rcmdr Package-----
#---------------------------------------------------
library(Rcmdr)


# ------------Non-Parametric Correlation Analysis-------------
#-------------------------------------------------------------
liarData = read.delim(file.choose(), header = TRUE)
View(liarData)

# Spearman’s correlation coefficient
cor.test(liarData$Position, liarData$Creativity, alternative = "less", 
          method = "spearman")

# Kendall’s tau
cor.test(liarData$Position, liarData$Creativity, alternative = "less", 
         method = "kendall")

# Bootstrapping correlations
library(boot)
'boot() what you want to bootstrap, and replications is a number specifying 
how many bootstrap samples you want to take (I usually set this value to 2000).'
bootTau<-function(liarData,i) cor(liarData$Position[i], 
                  liarData$Creativity[i], method = "kendall")

boot_kendall<-boot(liarData, bootTau, 2000)
boot_kendall
boot.ci(boot_kendall, conf = 0.95)


#-------Point Biserial-----
catData = read.csv("pbcorr.csv",  header = TRUE)
cor.test(catData$time, catData$gender)
cor.test(catData$time, catData$recode)

#---Biserial----
library(polycor)
polyserial(catData$time, catData$gender)


#-------Partial Correlation-----
library(ggm)
examData = read.delim("Exam Anxiety.dat", header = TRUE)
examData2 <- examData[, c("Exam", "Anxiety", "Revise")]

# pcor(c("var1", "var2", "control1", "control2" etc.), var(dataframe))
pc<-pcor(c("Exam", "Anxiety", "Revise"), var(examData2))
pc
pc^2

# pcor.test(pcor object, number of control variables, sample size)
pcor.test(pc, 1, 103)





