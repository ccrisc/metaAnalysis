l ## ------------------------------------------------------------------------
library(meta)
library(stargazer)
library(metafor)
#devtools::install_github("MathiasHarrer/dmetar")
library(dmetar)

## ------------------------------------------------------------------------
dat = read.csv("md.csv", sep = ';')[1:10,]
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



forest(fit)
funnel(fit)
plot(se,mean)


##--- Between study heterogeneity
#https://bookdown.org/MathiasHarrer/Doing_Meta_Analysis_in_R/heterogeneity.html
find.outliers(fit)




#Subgroup analyses
#https://bookdown.org/MathiasHarrer/Doing_Meta_Analysis_in_R/metareg.html
m.gen.reg <- metareg(fit, ~year)
bubble(m.gen.reg, studlab = TRUE)
metareg(fit, RiskOfBias)


