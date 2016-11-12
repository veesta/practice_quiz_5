library(tidyverse)
library(apaTables)
library(haven)

my.data <- read_spss("Lecture 7 regression_example_data.sav")

glimpse(my.data)

#correlation matrix

apa.cor.table(my.data)

apa.cor.table(my.data, filename="Table1_APA.doc", table.number=1)

#check curvilinear relationships 

psych::pairs.panels(as.data.frame(my.data))

#as of now, best predictor may be assessment centre ratings, r = .37

#examine single regressions - does DV2 contribute to prediction of job perf beyond DV1
#IV = jobperf
#DV1 = gma
#DV2 = con, ac, graph

#gma alone
regression.gma <- lm(jobperf ~ gma, data=my.data)
summary(regression.gma)
apa.reg.table(regression.gma)

regression.1.con <- lm(jobperf ~ gma + con, data=my.data)
summary(regression.1.con)
apa.reg.table(regression.1.con)

#R2=.36, 95% CI[.29,.41], F(2,497)=137.5, p<.001
#sr2=.10, 95% CI[.05, .14] 

regression.2.ac <- lm(jobperf ~ gma + ac, data=my.data)
summary(regression.2.ac)
apa.reg.table(regression.2.ac)

regression.3.graph <- lm(jobperf ~ gma + graph, data=my.data)
summary(regression.3.graph)
apa.reg.table(regression.3.graph)

##PART 2: Using two block regressions 

block1 = lm(jobperf ~ gma, data=my.data)

block2.con = lm(jobperf ~ gma + con, data=my.data)

apa.reg.table(block1,block2.con)




block4.graph = lm(jobperf ~ gma + graph, data=my.data)

apa.reg.table(block1,block4.graph)

##QUESTION 3 

#what is mean GMA and mean conscientiousness 

apa.cor.table(my.data)

#gma mean = 100.00
#con mean = 120.00

range <- data.frame(gma = c(100), con = c(120))

CI_data <- predict(my.regression,
                   newdata = range, interval = "confidence", level = 0.95)

