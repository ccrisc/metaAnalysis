## ------ LIBRARIES ------
library(meta)
library(stargazer)
library(metafor)
#devtools::install_github("MathiasHarrer/dmetar")
library(dmetar)
library(tidyverse)
library(PerformanceAnalytics)
library(grid)


## ------ PREPARE DATA ------
dat = read.csv("md.csv", sep = ';')
#dat = as.data.frame(dat)
dat[is.na(dat)] <- 0
dat$se <- as.numeric(gsub(",", ".", dat$se, fixed = TRUE))
dat$mean <- as.numeric(gsub(",", ".", dat$mean, fixed = TRUE))
dat$obs <- as.numeric(gsub(",", "", dat$obs, fixed = TRUE))
dat$subgroup <- factor(dat$extra_geo %in% c("US", "North America", "EU"),
                       labels = c("developing country", "developed country"))
dat$gini <- as.numeric(gsub(",", ".", dat$gini, fixed = TRUE))
dat$gdp <- as.numeric(gsub(",", ".", dat$gdp, fixed = TRUE))
#dat$subgroup <- c(1,0,0,1,1,0,0,1,0,0)
#dat$subgroup <- dat$extra_geo

str(dat)


## ------ BASIC META REGRESSION ------
attach(dat)
fit <- metagen(mean, 
               se,
               data=dat,
               studlab=paste0(study,'(',year,')'),
               sm="MD")
sink("metaresults.txt")
print(summary(fit), round = 2)
sink()
forest(fit)
plot(se,mean)


## ------ ROBUSTNESS CHECK (check heterogeneity) ------
find.outliers(fit) #outliers
m.gen.inf <- InfluenceAnalysis(fit, random = TRUE)
plot(m.gen.inf, "influence") #influence diagnostic
# See plot covariance ratio, if <1 indicates that removing study k results in a more precise estimate of the pooled effect size

plot(m.gen.inf, "es") #leave-One-Out: plot the overall effect and I^2 heterogeneity of all meta-analyses
#we see the recalculated pooled effects, with one study omitted each time
plot(m.gen.inf, "i2")

## ------ PUBBLICATION BIAS ------
funnel(fit)
funnel.meta(fit, xlim = c(-0.3, 1),
            contour = c(0.9, 0.95, 0.99),
            col.contour = c("gray75", "gray85", "gray95"))
legend(x = 0.6, y = 0.01, 
       legend = c("p < 0.1", "p < 0.05", "p < 0.01"),
       fill = c("gray75", "gray85", "gray95"))
title("Contour-Enhanced Funnel Plot")
sink("Egger test.txt")
metabias(fit) #Egger's test of the intercept quantifies funnel plot asymmetry and performs a statistical test (Egger et al., 1997).
sink()


## ------ SUBGROUP ANALYSIS based on GDP per capita (subgroup) ------
region_subgroup_common<-update.meta(fit, 
                                    byvar=subgroup)
#sink("region_subgroupGDP_common.txt")
region_subgroup_common

forest(region_subgroup_common)
grid.text("Forest plot by GDP subgroup", .5, .9, gp=gpar(cex=2))



## ------ SUBGROUP ANALYSIS based on Gini (subgroup) ------
dat$subgroup <- c(1,0,0,1,1,0,0,1,0,0)
attach(dat)
fit <- metagen(mean, 
               se,
               data=dat,
               studlab=paste0(study,'(',year,')'),
               sm="MD")
region_subgroup_common<-update.meta(fit, 
                                    byvar=subgroup)
#sink("region_subgroupGini_common.txt")
region_subgroup_common

forest(region_subgroup_common)
grid.text("Forest plot by Gini index subgroup", .5, .9, gp=gpar(cex=2))



## ------ SUBGROUP ANALYSIS based on Region (subgroup) ------
dat$subgroup <- dat$extra_geo
attach(dat)
fit <- metagen(mean, 
               se,
               data=dat,
               studlab=paste0(study,'(',year,')'),
               sm="MD")
region_subgroup_common<-update.meta(fit, 
                                    byvar=subgroup)
#sink("region_subgroupGini_common.txt")
region_subgroup_common
forest(region_subgroup_common)
grid.text("Forest plot by Geographical area subgroup", .5, .9, gp=gpar(cex=2))


## ------ META REGRESSION ------
#sink("metareg_control.txt")
#metareg(fit,subgroup)

#test if gini had affected the estimates of effect sizes.
model_gini <- metagen(mean,se,studlab=paste0(study,'(',year,')'),data=dat)
output_gini <- metareg(model_gini, ~gini)
#sink("metareg_gini.txt")
output_gini
bubble(output_gini,
       xlab = "Gini index",
       col.line = "blue",
       #cex = c(seq(0.8, 1.7, by=0.1)),
       studlab = FALSE)
title("Gini influence on estimates")

#test if publication year had affected the estimates of effect sizes.
model_pub_year <- metagen(mean,se,studlab=paste0(study,'(',year,')'),data=dat)
output_pub_year <- metareg(model_pub_year, ~year)
#sink("metareg_gini.txt")
output_pub_year
bubble(output_pub_year,
       xlab = "Publication year",
       col.line = "blue",
       #cex = c(seq(0.8, 1.7, by=0.1)),
       studlab = FALSE)
title("Publication year influence on estimates")



#multi model interference
dat[,c("gdp", "gini", "h_journal", 'h_country', 'obs', 'year')] %>% cor()
dat[,c("gdp", "gini", "h_journal", 'h_country', 'obs', 'year')] %>% 
  chart.Correlation()
multimodel.inference(TE = "mean", 
                     seTE = "se",
                     data = dat,
                     predictors = c("gdp", "gini", "h_journal", 'h_country', 'obs', 'year', 'sample_region'),
                     eval.criterion = 'BIC',
                     interaction = FALSE)


## ------ GENERATE TABLE ------
Table2 <- data.frame(`Authors/year of publication` = paste0(dat[,'study'],' (',dat[,'year'],')'),
                     `Region of interest` = dat[,'region'],
                     `Sample size` =  dat[,'obs'],
                     `Period` = dat[,'period'],
                     `Study population` = dat[,'population'],
                     `Nature of treatment` = dat[,'treatment'],
                     `Outcome` = dat[,'outcome'],
                     `Statistical method` = dat[,'method'],
                     `Treatment effect` = dat[,'mean'],
                     `Standard error` = dat[,'se'], 
                     `Ref. effect` = paste0('Page ', page, ', ', table),
                     check.names = FALSE)
stargazer(Table2, 
          summary=FALSE, 
          rownames=FALSE,
          title="Table 2: Studies included in the Meta-Analysis", 
          out="table2.html")

#-------
tbl2 = read.csv("discarded.csv", sep = ';')
Table1 <- data.frame(`Authors/year of publication` = paste0(tbl2 [,'study'],' (',tbl2 [,'year'],')'),
                     `Region of interest` = tbl2 [,'region'],
                     `Reason for exclusion` = tbl2 [,'why'], 
                     check.names = FALSE)

stargazer(Table1, 
          summary=FALSE, 
          rownames=FALSE,
          title="Table 1: Studies not included in the Meta-Analysis", 
          out="table1.html")

#-------
Table3 <- data.frame(`Authors/year of publication` = paste0(dat[,'study'],' (',dat[,'year'],')'),
                     `Journal` = dat[,'journal'],
                     `h-index journal` =  dat[,'h_journal'],
                     `h-index country` = dat[,'h_country'],
                     `Gini index` = dat[,'gini'],
                     `GDP per capita` = dat[,'gdp'],
                     check.names = FALSE)
stargazer(Table3, 
          summary=FALSE, 
          rownames=FALSE,
          title="Table 3: additional data retrieved for the studies", 
          out="table3.html")