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
dat$gdp <- dat$se <- as.numeric(gsub(",", ".", dat$gdp, fixed = TRUE))
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
m.gen.inf <- InfluenceAnalysis(m.gen, random = TRUE)
plot(m.gen.inf, "influence") #influence diagnostic
# See plot covariance ratio, if <1 indicates that removing study k results in a more precise estimate of the pooled effect size

plot(m.gen.inf, "es") #leave-One-Out: plot the overall effect and I^2 heterogeneity of all meta-analyses
#we see the recalculated pooled effects, with one study omitted each time


## ------ PUBBLICATION BIAS ------
funnel(fit)
funnel.meta(m.gen, xlim = c(-0.3, 1),
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
sink("region_subgroup_common.txt")
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
sink("metareg_control.txt")
metareg(fit,subgroup)

#test if gini had affected the estimates of effect sizes.
model_pub_year <- metagen(mean,se,studlab=paste0(study,'(',year,')'),data=dat)
output_pub_year <- metareg(model_pub_year, year)
sink("metareg_gini.txt")
output_pub_year
bubble(output_pub_year,
               xlab = "Publication Year",
               col.line = "blue",
               studlab = TRUE)



#multi regression
dat[,c("gdp", "gini", "year")] %>% cor()
dat[,c("extra_h_geo", "extra_h_journal", "year")] %>% 
  chart.Correlation()
multimodel.inference(TE = "mean", 
                     seTE = "se",
                     data = dat,
                     predictors = c("year", "extra_h_geo", "extra_h_journal", "extra_geo"),
                     interaction = FALSE)














#Table 1 with all required infos
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


df_fit <- summary(fit)

#Table 2 
results <- data.frame(`Study` = df_fit$studlab,
                        `MD` = round(df_fit$TE, 2),
                        `95% CI` = paste0('[', round(df_fit$lower,2), ' ; ', round(df_fit$upper, 2),']'),
                        `W(common)` = round(df_fit$w.fixed, 2),
                        `W(random)` = round(df_fit$w.random, 2),
                        check.names = FALSE)
stargazer(results, 
          summary=FALSE, 
          rownames=FALSE,
          title="Table 2: FE and RE meta-analysis based on estimates and their standard errors", 
          notes = paste0("Number of studies combined: k = ", df_fit$k),
          out="table2.html")

#####
results2 <- data.frame(`Model` = c('Common effect model', 'Random effect model'),
                      `MD` = round(c(df_fit$TE.fixed, df_fit$TE.random),  4),
                      `95% CI` = c(paste0('[', round(df_fit$lower.fixed,4), ' ; ', round(df_fit$upper.fixed, 4),']'),
                                   paste0('[', round(df_fit$lower.random,4), ' ; ', round(df_fit$upper.random, 4),']')),
                      `z` = round(c(df_fit$zval.fixed,df_fit$zval.random), 2),
                      `p-value` = round(c(df_fit$pval.fixed,df_fit$pval.random), 2),
                      check.names = FALSE)
stargazer(results2, 
          summary=FALSE, 
          rownames=FALSE,
          title="Table 3: Common/random effect models",
          out="table3.html")
#####



#multi regression
dat[,c("extra_h_geo", "extra_h_journal", "year")] %>% cor()
dat[,c("extra_h_geo", "extra_h_journal", "year")] %>% 
  chart.Correlation()
multimodel.inference(TE = "mean", 
                     seTE = "se",
                     data = dat,
                     predictors = c("year", "extra_h_geo", "extra_h_journal", "extra_geo"),
                     interaction = FALSE)

## Pubblication bias
fit.fun <- metagen(mean, se, studlab=study, data=dat, sm="MD")
funnel(fit.fun, studlab = TRUE, xlim = c(-0.3,1))
# Add title
title("Funnel Plot")
#shows the effect size of each study
#The vertical line in the middle of the funnel shows the average effect size


#- inspect how asymmetry patterns relate to statistical significance
#help to distinguish publication bias from other forms of asymmetry
# Define fill colors for contour
col.contour = c("gray75", "gray85", "gray95")
# Generate funnel plot (we do not include study labels here)
funnel.meta(m.gen, xlim = c(-0.3, 1),
            contour = c(0.9, 0.95, 0.99),
            col.contour = col.contour)
# Add a legend
legend(x = 0.7, y = 0.01, 
       legend = c("p < 0.1", "p < 0.05", "p < 0.01"),
       fill = col.contour)
# Add a title
title("Contour-Enhanced Funnel Plot")
#interested in the p<0.05 and p<0.01 regions, because effect sizes falling into this area are traditionally considered significant.




