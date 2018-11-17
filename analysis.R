library(readr)
library(dplyr)
library(modelr)
library(caret)
library(formattable)
library(corrplot)
library(GGally)

ds = read_csv("companies_data.csv")

# Fix categorical variables
ds$Default    = as.factor(ds$Default)
ds$nace       = as.factor(ds$nace)
ds$nace_group = as.factor(ds$nace_group)


# print name of every column
names(ds)

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
corrplot(cmatrix, method='circle')
corrplot(cmatrix, method='square')
corrplot(cmatrix, method='ellipse')
corrplot(cmatrix, method='shade')
# corrplot(cmatrix, type='upper', order='hclust', p.mat=p.mat, sig.level=0.01)

# Summary Statistics
summary(ds)


ds = ds %>% select(-c('nace', 'nace_group', 'Default'))

tmp <- do.call(data.frame, 
           list(mean = apply(ds, 2, mean),
                sd = apply(ds, 2, sd),
                median = apply(ds, 2, median),
                min = apply(ds, 2, min),
                max = apply(ds, 2, max),
                ))


# Fancy Stuff
widget_formattable = formattable(tmp, list(
    mean = color_tile('lightblue', 'white'),
    sd = color_tile('lightblue', 'white'),
    median = color_bar('red', na.rm = TRUE),
    min = color_tile('white', 'orange'),
    max = color_tile('white', 'orange')
    #area(col=c(intensity_1, intensity_2, intensity_3, intensity_4)) ~ normalize_bar("pink")
))
widget_formattable

ggparcoord(ds, columns = 1:11,  scale = 'globalminmax')
