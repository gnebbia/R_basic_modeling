library(readr)
library(dplyr)
library(modelr)
library(caret)
library(formattable)
library(corrplot)
library(GGally)
library(randomForest)

ds = read_csv("companies_data.csv")

# Fix categorical variables
ds$Default    = as.factor(ds$Default)
ds$nace       = as.factor(ds$nace)
ds$nace_group = as.factor(ds$nace_group)


# print name of every column
names(ds)

ds1 = ds %>% select(-c('Default','nace','nace_group'))

plot(ds1)

regmodel = lm(total_asset_total_liab ~ poly(debt_ratio,2), ds1)
summary(regmodel)
plot(total_asset_total_liab ~ debt_ratio, ds1)
points(ds$debt_ratio, fitted(regmodel), col='red')

# Plot of distributions
hist(ds$leverage_ratio, col='red', breaks=40, main='Leverage Ratio Distribution', xlab='Leverage Ratio')
hist(ds$ROI, col='red', breaks=40, main='ROI Distribution', xlab='Leverage Ratio')
hist(ds$ROCE, col='red', breaks=40, main='ROCE Distribution', xlab='Leverage Ratio')
hist(ds$quick_ratio, col='red', breaks=40, main='Quick Ratio Distribution', xlab='Leverage Ratio')
hist(ds$asset_turnover, col='red', breaks=40, main='Asset Turnover Distribution', xlab='Leverage Ratio')


# Boxplot
boxplot(ds$ROI, col='lightblue', main='ROI Boxplot')
boxplot(ds$ROCE, col='lightblue', main='ROCE Boxplot')

# Boxplot depending on Default
boxplot(ROI ~ Default, ds, col='lightblue', xlab='Default', main='ROI vs Default Boxplot')


# Scatter Plot
plot(ds)

plot(ds$total_asset_total_liab, ds$debt_ratio, col='lightgreen')
plot(ds$ROI, ds$ROE, col='red')
plot(ds$ROS, ds$ROCE, col='blue')


# Correlation Matrix
cmatrix = cor(ds %>% select(-c('Default','nace','nace_group')))

corrplot(cmatrix, method='circle', diag=F, type='upper')
corrplot(cmatrix, method='square')
corrplot(cmatrix, method='ellipse')
corrplot(cmatrix, method='shade')


cmatrix_ng1 = cor(ds %>% filter(nace_group == 1) %>% select(-c('Default','nace','nace_group')))
cmatrix_ng2 = cor(ds %>% filter(nace_group == 2) %>% select(-c('Default','nace','nace_group')))
cmatrix_ng3 = cor(ds %>% filter(nace_group == 3) %>% select(-c('Default','nace','nace_group')))

par(mfrow=c(1,3))
corrplot(cmatrix_ng1, method='circle')
corrplot(cmatrix_ng2, method='circle')
corrplot(cmatrix_ng3, method='circle')

par(mfrow=c(1,1))
# corrplot(cmatrix, type='upper', order='hclust', p.mat=p.mat, sig.level=0.01)

# Summary Statistics
summary(ds)


ds = ds %>% select(-c('nace', 'nace_group', 'Default'))

dsc = ds %>% select(c('nace_group'))

tmp <- do.call(data.frame, 
           list(table(dsc)                ))

tmp = as.data.frame(table(ds$nace_group))
tmp <- do.call(data.frame, 
           list(mean = apply(ds, 2, mean),
                sd = apply(ds, 2, sd),
                median = apply(ds, 2, median),
                min = apply(ds, 2, min),
                max = apply(ds, 2, max)
                ))

tmp <- do.call(data.frame, 
           list(mean = apply(ds, 2, mean),
                sd = apply(ds, 2, sd),
                median = apply(ds, 2, median),
                min = apply(ds, 2, min),
                max = apply(ds, 2, max)
                ))


widget_formattable = formattable(tmp, list(
    mean = color_tile('lightblue', 'white'),
    sd = color_tile('lightblue', 'white'),
    median = color_bar('red', na.rm = TRUE),
    min = color_tile('white', 'orange'),
    max = color_tile('white', 'orange')
    #area(col=c(intensity_1, intensity_2, intensity_3, intensity_4)) ~ normalize_bar("pink")
))
widget_formattable

# Fancy Stuff
widget_formattable = formattable(tmp, list(
    Freq = color_tile('lightblue', 'white')
))
widget_formattable

ggparcoord(ds, columns = 1:11,  scale = 'globalminmax')



