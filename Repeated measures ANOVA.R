
##### Repeated measures ANOVA #####



### Load packages

library(ez)
library(reshape2)
library(pastecs)


# ----- One between-subjects variable and one within-subjects variable 

# Data prep -----------------------------------------------------------------------------------------------------------------------
data <- read.csv(file.choose())
data$id <- seq.int(nrow(data))

# Need to convert data into long format
data.long <- reshape(data, varying=2:3, v.names="dv", timevar="interval", times=1:2,
                     idvar="id", direction="long")        #timevar: within subject/repeated condition;  #times: number of repeated condition; #v.names = dependent variable; #varying = where repeated condition is. 
data.long$id <- as.factor(data.long$id)
data.long$Group <- as.factor(data.long$Group)
data.long$interval <- as.factor(data.long$interval)
datag$Group <- factor(data$Group,
                     levels = c(1,2,3),
                     labels = c("Control", "Exp1", "Exp2")) #Group: between subject condition 
attach(data.long)

by(data.long$dv, data.long$interval, stat.desc)
by(data.long$dv, data.long$Group, stat.desc)
by(data.long$dv, list(data.long$interval, data.long$Group), stat.desc, basic=FALSE)

# RM ANOVA Main analysis--------------------------------------------------------------------------------------------------------------


RM.data <- ezANOVA(data=data.long, dv=.(dv), wid=.(id), within=interval,
                   between=Group, type=3, detailed=T)
RM.data

### Simple effect
#subsample analysis (but at the cost of losing sample size...)
data.con.long <- data.long[ which(data.long$Group==1),]; data.con.long

# Run a two-way ANOVA
attach(data.con.long)
data.twoway <- anova(lm(dv ~ interval, data=data.con.long)); data.twoway


##main ANOVA using EZ package
dt <- ezANOVA(data=data.long, dv=.(dv), wid=.(id), within=.(interval), between =.(group), detailed=TRUE, type=3)
dt






#----- Two between-subjects variable and one within-subjects variable
data2 <- read.csv(file.choose())

# Data prep -----------------------------------------------------------------------------------------------------------------------

# Need to convert data into long format
data2.long <- reshape(data2, varying=2:3, v.names="freq", timevar="interval", times=1:2,
                     idvar="Subject", direction="long")  #timevar: within subject/repeated condition;  #times: number of repeated condition; #v.names = dependent variable;
data2.long$Subject <- as.factor(Lawrence.long$Subject)
data2.long$Condition <- as.factor(Lawrence.long$Condition)
data2.long$Sex <- as.factor(Lawrence.long$Sex)
data2.long$interval <- as.factor(Lawrence.long$interval)

data2.long$Condition <- factor(Lawrence.long$Condition,
                     levels = c(1,2),
                     labels = c("control", "exp"))
data2.long$Sex <- factor(Lawrence.long$Sex,
                                  levels = c(1,2),
                                  labels = c("Male", "Female"))  #sex and condition : between subject variable

attach(data2.long)#; head(Lawrence.long)
by(data2.long$freq, data2.long$interval, stat.desc)
by(data2.long$freq, data2.long$Condition, stat.desc)
by(data2.long$freq, data2.long$Sex, stat.desc)
by(data2.long$freq, list(data2.long$interval, data2.long$Condition, data2.long$Sex), 
                stat.desc, basic=FALSE)

# Two between-subjects variable and one-within-subjects ANOVA
RM.data2 <- ezANOVA(data=data2.long, dv=.(freq), wid=.(Subject), within=interval,
                   between=.(Condition,Sex), type=3, detailed=T)
RM.data2



### simple effects --> simple two-way ANOVA within a specified group

# At pretest -- Select subsample
data2.pre.long <- Lawrence.long[ which(Lawrence.long$interval==1),]; data2.pre.long

# Run a two-way ANOVA
attach(data2.pre.long)
data.2 <- anova(lm(freq~Condition+Sex+Condition:Sex, data=data2.pre.long)); data.2

# Run a two between-subjects and one within-subjects ANOVA (three way anova)
data2.rm3 <- ezANOVA(data=data2.long, dv=.(freq), wid=.(Subject), within=interval,
                       between=.(Sex,condition), type=3, detailed=T)
data2.rm3





#---Intraclass correlation------------------------------------------------------------------------------------
ICC <- read.csv(file.choose()); ICC

# Need to convert data into long format
ICC.long <- melt(ICC, id="id", measured=c("measure1", "measure2", "measure3"))
ICC.long; names(ICC.long) <- c("id", "Rater", "measure"); ICC.long
ICC.long$Child <- as.factor(ICC.long$id)
ICC.long$Rater <- as.factor(ICC.long$Rater)
# Descriptive Statistics
by(ICC.long$measure, ICC.long$id, summary)


# RM ANOVA
RM.ICC <- ezANOVA(data=ICC.long, dv=.(measure), wid=.(id), within=.(Rater), detailed=TRUE, type=3)
RM.ICC


