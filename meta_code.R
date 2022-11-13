## ------ LIBRARIES ------
library(meta)
library(stargazer)
library(metafor)
#devtools::install_github("MathiasHarrer/dmetar")
library(dmetar)
library(tidyverse)
library(PerformanceAnalytics)


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
str(dat)


## ------ BASIC META REGRESSION ------
attach(dat)
fit <- metagen(mean, 
               se,
               data=dat,
               studlab=paste0(study,'(',year,')'),
               comb.fixed = FALSE,
               comb.random = TRUE,
               method.tau = "SJ",
               hakn = TRUE,
               #prediction=TRUE,
               sm="MD")
print(summary(fit), round = 2)
forest(fit)
plot(se,mean)


## ------ ROBUSTNESS CHECK (check heterogeneity) ------
find.outliers(fit) #outliers
m.gen.inf <- InfluenceAnalysis(fit, random = TRUE)
plot(m.gen.inf, "influence") #influence diagnostic
# See plot covariance ratio, if <1 indicates that removing study k results in a more precise estimate of the pooled effect size

plot(m.gen.inf, "es") #leave-One-Out: plot the overall effect and I^2 heterogeneity of all meta-analyses
#we see the recalculated pooled effects, with one study omitted each time


## ------ PUBBLICATION BIAS ------
funnel(fit)
funnel.meta(fit, xlim = c(-0.3, 1),
            contour = c(0.9, 0.95, 0.99),
            col.contour = c("gray75", "gray85", "gray95"))
legend(x = 0.6, y = 0.01, 
       legend = c("p < 0.1", "p < 0.05", "p < 0.01"),
       fill = c("gray75", "gray85", "gray95"))
title("Contour-Enhanced Funnel Plot")

metabias(fit) #Egger's test of the intercept quantifies funnel plot asymmetry and performs a statistical test (Egger et al., 1997).


## ------ SUBGROUP ANALYSIS based on GDP per capita (subgroup) ------
region_subgroup_common<-update.meta(fit, 
                                    byvar=subgroup, 
                                    comb.random = TRUE, 
                                    comb.fixed = TRUE,
                                    tau.common=TRUE)
#sink("region_subgroup_common.txt")
region_subgroup_common

forest(region_subgroup_common,
       #sortvar = as.Date(dat$subgroup_GDP),
       allstudies = TRUE,
       comb.fixed=TRUE,
       comb.random = TRUE,
       hetstat=FALSE,
       test.effect.subgroup.random = TRUE,
       col.by="dark gray",
       label.test.effect.subgroup.random="Subgroup effect",
       test.subgroup.random = TRUE)




## ------ META REGRESSION ------
#sink("metareg_control.txt")
metareg(fit,subgroup)

#test if gini had affected the estimates of effect sizes.
model_pub_year <- metagen(mean,se,studlab=paste0(study,'(',year,')'),data=dat)
output_pub_year <- metareg(model_pub_year, year)
#sink("metareg_gini.txt")
output_pub_year
bubble(output_pub_year,
               xlab = "Publication Year",
               col.line = "blue",
               studlab = TRUE)



#multi model interference
dat[,c("gdp", "gini", "year")] %>% cor()
dat[,c("gdp", "gini", "year")] %>% 
  chart.Correlation()
multimodel.inference(TE = "mean", 
                     seTE = "se",
                     data = dat,
                     predictors = c("gdp", "gini", "year"),
                     interaction = FALSE)


## ------ GENERATE TABLE ------
df_tables <- data.frame(`Authors/year of publication` = paste0(dat[,'study'],' (',dat[,'year'],')'),
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
stargazer(df_tables, 
          summary=FALSE, 
          rownames=FALSE,
          title="Table 1: Studies included in the Meta-Analysis", 
          out="table1.html")




