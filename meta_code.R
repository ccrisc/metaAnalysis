 ## ------------------------------------------------------------------------
library(meta)
library(stargazer)
library(metafor)
#devtools::install_github("MathiasHarrer/dmetar")
library(dmetar)
library(tidyverse)
library(PerformanceAnalytics)

## ------------------------------------------------------------------------
dat = read.csv("md.csv", sep = ';')
#dat = as.data.frame(dat)


#Table 1 with all required infos
df_tables <- data.frame(`Authors/year of publication` = paste0(dat[,'study'],' (',dat[,'year'],')'),
                        `Region of interest` = dat[,'region'],
                        `Sample size` =  dat[,'obs'],
                        `Period` = dat[,'period'],
                        `Study population` = dat[,'population'],
                        #`Nature of treatment` = dat[,'treatment'],
                        #`Outcome` = dat[,'outcome'],
                        #`Statistical method` = dat[,'method'],
                        `Treatment effect` = dat[,'mean'],
                        `Standard error` = dat[,'se'], 
                        #`Ref. effect` = paste0('Page ', page, ', ', table),
                        check.names = FALSE)
stargazer(df_tables, 
          summary=FALSE, 
          rownames=FALSE,
          title="Table 1: Studies included in the Meta-Analysis", 
          out="table1.html")



## ------------------------------------------------------------------------
attach(dat)
str(dat)
#dat$se <- as.numeric(dat$se)
dat$se <- as.numeric(gsub(",", ".", dat$se, fixed = TRUE))
dat$mean <- as.numeric(gsub(",", ".", dat$mean, fixed = TRUE))
dat$obs <- as.numeric(gsub(",", "", dat$obs, fixed = TRUE))
str(dat)


fit <- metagen(mean, se, studlab=paste0(study,'(',year,')'), data=dat, sm="MD")
print(summary(fit), round = 2)
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


##--- Between study heterogeneity
m.gen <- update.meta(fit, prediction = TRUE)
#this function indicates the outliers
find.outliers(m.gen)
#https://bookdown.org/MathiasHarrer/Doing_Meta_Analysis_in_R/heterogeneity.html
m.gen.inf <- InfluenceAnalysis(m.gen, random = TRUE)
# 1) influence diagnostic
plot(m.gen.inf, "influence")
# See plot covariance ratio, if <1 indicates that removing study k results in a more precise estimate of the pooled effect size

# 2) leave-One-Out 
#plot the overall effect and I^2 heterogeneity of all meta-analyses
plot(m.gen.inf, "es")
#we see the recalculated pooled effects, with one study omitted each time
#ES plot is ordered by effect size
#when squae is on the left studies have very high effect sizes, we find that the overall effect is smallest when they are removed.
plot(m.gen.inf, "i2")
#ordered by heterogeneity (low to high)

#STUDIES HOWEVER DO NOT POINT IN THE SAME DIRECTION
#So we cannot say studies which are likely influential outliers


## forest plot
forest(fit)
plot(se,mean)

## Subgroup analyses
#https://bookdown.org/MathiasHarrer/Doing_Meta_Analysis_in_R/metareg.html
m.gen.reg <- metareg(fit, ~year)
bubble(m.gen.reg, studlab = TRUE)
metareg(fit, RiskOfBias)


# Show first entries of study name and 'extra_geo' column
head(dat[,c("study", "extra_geo")])
subgroup.meta <- update.meta(fit, 
                             subgroup = extra_geo, 
                             tau.common = FALSE)

## Meta regression
m.gen.reg <- metareg(subgroup.meta, ~year)
bubble(m.gen.reg, studlab = TRUE)


#multi regression
dat[,c("", "extra_h_geo", "year")] %>% cor()
MVRegressionData[,c("reputation", "quality", "pubyear")] %>% 
  chart.Correlation()
multimodel.inference(TE = "mean", 
                     seTE = "se",
                     data = MVRegressionData,
                     predictors = c("pubyear", "quality", 
                                    "reputation", "continent"),
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
legend(x = 0.6, y = 0.01, 
       legend = c("p < 0.1", "p < 0.05", "p < 0.01"),
       fill = col.contour)
# Add a title
title("Contour-Enhanced Funnel Plot")
#interested in the p<0.05 and p<0.01 regions, because effect sizes falling into this area are traditionally considered significant.


