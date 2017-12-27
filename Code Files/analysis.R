require(dummies)
require(stargazer)
require(sandwich)
require(dplyr)
require(lubridate)
require(reshape2)
require(plm)
require(ggplot2)

df <- read.csv2('Data/cleaned_dataset.csv', sep = ',', dec = '.', colClasses = 'character')

# convert to proper data types
df$Datetime_UTC <- as.POSIXct(df$Datetime_UTC, tz = 'UTC')
df$Fuel_Type <- as.factor(df$Fuel_Type)
df[,3:9] <- sapply(df[,3:9], FUN = as.numeric)

# RESHAPE DATA

# reshape emissions data
df <- melt(df, id.vars = c('Datetime_UTC','Fuel_Type','Gen_MW','dGen_MW','Load_MW','Temp_F'), 
                  measure.vars = c('SO2_tons', 'NOx_tons', 'CO2_tons'), 
                  variable.name = 'Emissions_Type', value.name = 'Emissions_tons')

# SUMMARY STATISTICS

# group load, generation and temperature by hour
hrly_summary <- df %>% group_by(Datetime_UTC) %>% 
    summarise(Gen_MW = mean(Gen_MW, na.rm=TRUE), dGen_MW = mean(dGen_MW, na.rm=TRUE),
              Load_MW = mean(Load_MW, na.rm=TRUE), Temp_F = mean(Temp_F, na.rm=TRUE))

# output summary stats
stargazer(data.frame(hrly_summary),
          type = 'text', title = 'Summary Statistics', out = './Tables/summary_stats1.html',
          covariate.labels = c('Generation (MW)','delta Generation (MW)','Load (MW)','Temperature (F)'))

stargazer(df[,c('Datetime_UTC','Emissions_tons')], type = 'text', title = 'Summary Statistics', 
          out = './Tables/summary_stats2.html', covariate.labels = c('Emissions (tons)'))

# PLOTS
# delta generation vs emissions (log scale)
df_sample <- dplyr::sample_n(df, 500)
plot(df_sample$dGen_MW[df_sample$Emissions_Type=='CO2_tons'], log(df_sample$Emissions_tons[df_sample$Emissions_Type=='CO2_tons']),
     xlab = 'Hourly Change in Generation (MW)', ylab = 'Emissions (log scale)', main = 'Emissions')
points(df_sample$dGen_MW[df_sample$Emissions_Type=='SO2_tons'], log(df_sample$Emissions_tons[df_sample$Emissions_Type=='SO2_tons']),
     col='blue')
points(df_sample$dGen_MW[df_sample$Emissions_Type=='NOx_tons'], log(df_sample$Emissions_tons[df_sample$Emissions_Type=='NOx_tons']),
     col='red')
legend('topleft',legend = c('Carbon Dioxide','Sulfur Dioxide', 'Nitrogen Oxides'), fill = c('black','blue','red'))

# log of generation vs emissions (log scale)
plot(log(df_sample$Gen_MW[df_sample$Emissions_Type=='CO2_tons']), log(df_sample$Emissions_tons[df_sample$Emissions_Type=='CO2_tons']),
     xlab = 'Generation (log scale)', ylab = 'Emissions (log scale)', main = 'Emissions')
points(log(df_sample$Gen_MW[df_sample$Emissions_Type=='SO2_tons']), log(df_sample$Emissions_tons[df_sample$Emissions_Type=='SO2_tons']),
       col='blue')
points(log(df_sample$Gen_MW[df_sample$Emissions_Type=='NOx_tons']), log(df_sample$Emissions_tons[df_sample$Emissions_Type=='NOx_tons']),
       col='red')
legend('topleft',legend = c('Carbon Dioxide','Sulfur Dioxide', 'Nitrogen Oxides'), fill = c('black','blue','red'))

# REGRESSIONS

# LINEAR MODEL

# BASE SPECIFICATION
# regress emissions on delta generation and dummies for fuel type, emissions type, and their interactions

lm_1 <- lm(df$Emissions_tons ~ df$dGen_MW + dummy('Fuel_Type',df) * dummy('Emissions_Type',df))

# calculate robust standard errors
lm_1.se <- sqrt(diag(vcovHC(lm_1, 'HC1')))

# output regression table with stargazer, supress output of interaction terms
stargazer(lm_1, se=list(lm_1.se), type = 'text', out = './Tables/LM1.html', 
          dep.var.labels = 'Emissions (Tons)', no.space = TRUE,
          covariate.labels = c('Hourly delta Gen. (MW)', as.character(unique(df$Fuel_Type)), 'SO2', 'NOx', 'CO2'),
          omit = 15:47, notes = 'Interaction terms omitted.')

# output regression table without dummies
stargazer(lm_1, se=list(lm_1.se), type = 'text', dep.var.labels = c('Emissions (Tons)'),
          covariate.labels = c('Hourly delta Gen. (MW)'), 
          out = './Tables/LM1-reduced.html', 
          omit = c('dummy'), omit.labels = c('Fuel/Emissions Dummies'))

# CONTROL FOR VARIATION IN LOAD

lm_2 <- lm(df$Emissions_tons ~ df$dGen_MW + df$Load_MW + dummy('Fuel_Type',df) * dummy('Emissions_Type',df))

# calculate robust standard errors
lm_2.se <- sqrt(diag(vcovHC(lm_2, 'HC1')))

# output regression table with stargazer, omitting dummy variables
stargazer(lm_1, lm_2, se=list(lm_1.se, lm_2.se), type = 'text', dep.var.labels = c('Emissions (Tons)'),
          covariate.labels = c('Hourly delta Gen. (MW)','Total Load (MW)'), 
          out = './Tables/LM2.html', 
          omit = c('dummy'), omit.labels = c('Fuel/Emissions Dummies'))

# ADD CONTROL FOR TEMPERATURE

lm_3 <- lm(df$Emissions_tons ~ df$dGen_MW + df$Load_MW + df$Temp_F +
               dummy('Fuel_Type',df) * dummy('Emissions_Type',df))

# calculate robust standard errors
lm_3.se <- sqrt(diag(vcovHC(lm_3, 'HC1')))

# output regression table with stargazer, omitting dummy variables
stargazer(lm_1, lm_2, lm_3, se=list(lm_1.se, lm_2.se, lm_3.se), type = 'text', dep.var.labels = c('Emissions (Tons)'),
          covariate.labels = c('Hourly delta Gen. (MW)','Total Load (MW)','Temperature (F)'), 
          out = './Tables/LM3.html', 
          omit = c('dummy'), omit.labels = c('Fuel/Emissions Dummies'))

# ADD CONTROL FOR SQUARE OF TEMPERATURE

df$Temp_F_2 <- df$Temp_F**2

lm_4 <- lm(df$Emissions_tons ~ df$dGen_MW + df$Load_MW + df$Temp_F + df$Temp_F_2 +
               dummy('Fuel_Type',df) * dummy('Emissions_Type',df))

# calculate robust standard errors
lm_4.se <- sqrt(diag(vcovHC(lm_4, 'HC1')))

# output regression table with stargazer, omitting dummy variables
stargazer(lm_1, lm_2, lm_3, lm_4, se=list(lm_1.se, lm_2.se, lm_3.se, lm_4.se), type = 'text', dep.var.labels = c('Emissions (Tons)'),
          covariate.labels = c('Hourly delta Gen. (MW)','Total Load (MW)','Temperature (F)', 'Square of Temp. (MW)'), 
          out = './Tables/LM4.html', 
          omit = c('dummy'), omit.labels = c('Fuel/Emissions Dummies'))

# ADD FIXED EFFECTS

# add entity index column to combine fuel & emissions types
df$Fuel_Emissions_Type <- paste(df$Fuel_Type,df$Emissions_Type,sep = ' - ')

fe_1 <- plm(Emissions_tons ~ dGen_MW + Load_MW + Temp_F + Temp_F_2
                  + dummy('Fuel_Type',df) * dummy('Emissions_Type',df), data = df,
            index = c('Fuel_Emissions_Type', 'Datetime_UTC'), model = 'within', effect = 'individual')

# calculate robust standard errors
fe_1.se <- sqrt(diag(vcovHC(fe_1, type = 'HC1')))

stargazer(lm_1, lm_2, lm_3, lm_4, fe_1, se=list(lm_1.se, lm_2.se, lm_3.se, lm_4.se, fe_1.se), 
          type = 'text', dep.var.labels = c('Emissions (Tons)'),
          covariate.labels = c('Hourly delta Gen. (MW)','Total Load (MW)','Temperature (F)', 'Square of Temp. (F)'), 
          out = './Tables/FE1.html', 
          omit = c('dummy'), omit.labels = c('Fuel/Emissions Dummies'))

# LINEAR-LOG MODEL

# BASE SPECIFICATION

# coal is the omitted fuel type dummy variable, CO2 is the omitted variable for emissions type

log_1 <- lm(df$Emissions_tons ~ log(df$Gen_MW) + dummy('Fuel_Type',df) * dummy('Emissions_Type',df))

# calculate robust standard errors
log_1.se <- sqrt(diag(vcovHC(log_1, 'HC1')))

# output regression table without dummies
stargazer(log_1, se=list(log_1.se), type = 'text', dep.var.labels = c('Emissions (Tons)'),
          covariate.labels = c('log(Generation)'), 
          out = './Tables/LOG1.html', 
          omit = c('dummy'), omit.labels = c('Fuel/Emissions Dummies'))

# CONTROL FOR VARIATION IN LOAD

log_2 <- lm(df$Emissions_tons ~ log(df$Gen_MW) + df$Load_MW + dummy('Fuel_Type',df) * dummy('Emissions_Type',df))

# calculate robust standard errors
log_2.se <- sqrt(diag(vcovHC(log_2, 'HC1')))

# output regression table with stargazer, omitting dummy variables
stargazer(log_1, log_2, se=list(log_1.se, log_2.se), type = 'text', dep.var.labels = c('Emissions (Tons)'),
          covariate.labels = c('log(Generation)','Total Load (MW)'), 
          out = './Tables/LOG2.html', 
          omit = c('dummy'), omit.labels = c('Fuel/Emissions Dummies'))

# ADD CONTROL FOR TEMPERATURE

log_3 <- lm(df$Emissions_tons ~ log(df$Gen_MW) + df$Load_MW + df$Temp_F +
               dummy('Fuel_Type',df) * dummy('Emissions_Type',df))

# calculate robust standard errors
log_3.se <- sqrt(diag(vcovHC(log_3, 'HC1')))

# output regression table with stargazer, omitting dummy variables
stargazer(log_1, log_2, log_3, se=list(log_1.se, log_2.se, log_3.se), type = 'text', dep.var.labels = c('Emissions (Tons)'),
          covariate.labels = c('log(Generation)','Total Load (MW)','Temperature (F)'), 
          out = './Tables/LOG3.html', 
          omit = c('dummy'), omit.labels = c('Fuel/Emissions Dummies'))

# ADD CONTROL FOR SQUARE OF TEMPERATURE

log_4 <- lm(df$Emissions_tons ~ log(df$Gen_MW) + df$Load_MW + df$Temp_F + df$Temp_F_2 +
               dummy('Fuel_Type',df)[,2:10] * dummy('Emissions_Type',df)[,1:2])

# calculate robust standard errors
log_4.se <- sqrt(diag(vcovHC(log_4, 'HC1')))

# output regression table with stargazer, omitting dummy variables
stargazer(log_1, log_2, log_3, log_4, se=list(log_1.se, log_2.se, log_3.se, log_4.se), type = 'text', 
          dep.var.labels = c('Emissions (Tons)'),
          covariate.labels = c('log(Generation)','Total Load (MW)','Temperature (F)', 'Square of Temp. (MW)'), 
          out = './Tables/LOG4.html', 
          omit = c('dummy'), omit.labels = c('Fuel/Emissions Dummies'))

# ADD FIXED EFFECTS

fe_2 <- plm(Emissions_tons ~ log(Gen_MW) + Load_MW + Temp_F + Temp_F_2
            + dummy('Fuel_Type',df) * dummy('Emissions_Type',df), data = df,
            index = c('Fuel_Emissions_Type', 'Datetime_UTC'), model = 'within', effect = 'individual')


# calculate robust standard errors
fe_2.se <- sqrt(diag(vcovHC(fe_2, type = 'HC1')))

stargazer(log_1, log_2, log_3, log_4, fe_2, se=list(log_1.se, log_2.se, log_3.se, log_4.se, fe_2.se), 
          type = 'text', dep.var.labels = c('Emissions (Tons)'),
          covariate.labels = c('log(Generation)', 'Total Load (MW)','Temperature (F)', 'Square of Temp. (F)'), 
          out = './Tables/FE2.html', 
          omit = c('dummy'), omit.labels = c('Fuel/Emissions Dummies'))

# output combined final regression table
stargazer(lm_1, log_1, lm_2, lm_4, log_4, fe_1, fe_2,
          se=list(lm_1.se, log_1.se, lm_2.se, lm_4.se, log_4.se, fe_1.se, fe_2.se),
          type = 'text', out = './Tables/Final.html',
          dep.var.labels = c('Emissions (Tons)', 'Emissions (Tons)'),
          omit = c('dummy'), omit.labels = c('Fuel/Emissions Dummies'),
          covariate.labels = c('Hourly delta Gen. (MW)', 'log(Generation)', 'Total Load (MW)',
                               'Temperature (F)', 'Square of Temp. (F)'))
