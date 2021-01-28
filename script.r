# we are using different libraries
library(corrplot)
library(purrr)
library(ggplot2)
library(hrbrthemes)
library(raster)
library(tidyverse)
library(viridis)

# loading in full dataset
fpath<-"housing.csv"
all_housing<-read.csv(fpath,header=TRUE,stringsAsFactors = FALSE)

# checking what the data is, how it looks like, and what does it include
dim(all_housing)
colnames(all_housing)
str(all_housing)
head(all_housing)
summary(all_housing)
View(all_housing)

levels(as.factor(all_housing$ocean_proximity))

# checking how many NULL values do we have in each column
colSums(is.na(all_housing))

# cleaning all the records with NULL values
cleaned <- all_housing[rowSums(is.na(all_housing)) == 0,]
colSums(is.na(cleaned))  # rechecking

# to calculate the correlation matrix we need to have only numeric columns
cleanedNumeric <- cleaned[ , purrr::map_lgl(cleaned, is.numeric)]
# calculating the correlation matrix
hcor <- cor(cleanedNumeric)

# plotting
corrplot(hcor, method='number')

"
The most interesting for us in the plotted matrix is the 'total_bedrooms' 
row/column, because we need to find the most correlated column, so that we can
replace the NULL values. As we see, it is the 'households' column, which is
quite expected, as each household needs a few bedrooms, so the variables
are highly correlated with positive sign: more households -> more bedrooms

As a matter of fact, we don't need so correlated columns/features in our dataset
because if we were trying to create the regression model, it would influence
the accuracy significantly. It is always much better not to have strongly
correlated feature.

So we use this column below to define the missing values
"

# calculating correlation coefficient and plotting
brcor <- cor(cleaned$total_bedrooms, cleaned$households)
brcor  # ~0.98, it means the dependence is almost linear
windowsFonts(
  A=windowsFont("Arial")
)
# plotting to see the the dependence
pl <- ggplot(cleaned, aes(x=households, y=total_bedrooms)) +
  labs(title="Total Bedrooms / Households Dependence", 
       x="Households", y="Total Bedrooms") +
  theme(plot.title=element_text(size=30, face="bold", family='sans'), 
        axis.text.x=element_text(size=15, family='sans'), 
        axis.text.y=element_text(size=15, family='sans'),
        axis.title.x=element_text(size=25, family='sans'),
        axis.title.y=element_text(size=25, family='sans')) +
  geom_point( color="#69b3a2" ) +
  theme_ipsum(base_family = 'sans')
pl
"
As the result we get 207 NULLs for the total_bedrooms column
I decided to replace them using regression model based on correlation with 
households, it means that we create this model, and if we meet record with 
NULL value, we replace it with the predicted value
"
bhmodel <- lm(total_bedrooms ~ households, cleaned)
bhmodel
summary(bhmodel)

qqnorm(bhmodel$residuals)
qqline(bhmodel$residuals)  # we have smth to do with this

shapiro.test(sample(bhmodel$residuals, 2000))

# plotting with regression model
pl2 <- ggplot(cleaned, aes(x=households, y=total_bedrooms)) +
  labs(title="Total Bedrooms / Households Dependence", 
       x="Households", y="Total Bedrooms") +
  theme(plot.title=element_text(size=30, face="bold"), 
        axis.text.x=element_text(size=15), 
        axis.text.y=element_text(size=15),
        axis.title.x=element_text(size=25),
        axis.title.y=element_text(size=25)) +
  geom_point( color="#69b3a2" ) +
  geom_smooth(method=lm , color="darkorange", se=FALSE) +
  theme_ipsum()
pl2

# replacing the missing values with predictions of our model
all_housing$total_bedrooms[is.na(all_housing$total_bedrooms)] = 
  predict(bhmodel, all_housing[is.na(all_housing$total_bedrooms), ])

# checking if the null values has left
colSums(is.na(all_housing))

"
So, we have to transform categorical values into numerical, and it would be
great to remove outliers, but again, we are not building a model for now, so
it is not neccessary, as they may be interesting for the statistical analysis.
"

# checking columns for categorical values
str(all_housing)

# we need to transform the 'ocean_proximity' column
# let's draw it first
states    <- c('California')
us <- getData("GADM",country="USA",level=1)
us.states <- us[us$NAME_1 %in% states,]
us.bbox <- bbox(us.states)
xlim <- c(min(us.bbox[1,1]),max(us.bbox[1,2]))
ylim <- c(min(us.bbox[2,1]),max(us.bbox[2,2]))


plot(us.states, xlim=xlim, ylim=ylim, family="sans", main="Ocean proximity")
points(all_housing$longitude[all_housing$ocean_proximity=="<1H OCEAN"], 
       all_housing$latitude[all_housing$ocean_proximity=="<1H OCEAN"], 
       col = "#F8756D", cex = .5)
points(all_housing$longitude[all_housing$ocean_proximity=="NEAR BAY"], 
       all_housing$latitude[all_housing$ocean_proximity=="NEAR BAY"], 
       col = "#00BE7D", cex = .5)
points(all_housing$longitude[all_housing$ocean_proximity=="INLAND"], 
       all_housing$latitude[all_housing$ocean_proximity=="INLAND"], 
       col = "#A3A500", cex = .5)
points(all_housing$longitude[all_housing$ocean_proximity=="ISLAND"], 
       all_housing$latitude[all_housing$ocean_proximity=="ISLAND"], 
       col = "#03B0F6", cex = .5)
points(all_housing$longitude[all_housing$ocean_proximity=="NEAR OCEAN"], 
       all_housing$latitude[all_housing$ocean_proximity=="NEAR OCEAN"], 
       col = "#E76AF3", cex = .5)
legend(legend = c("<1H OCEAN", "NEAR BAY", "INLAND", "ISLAND", "NEAR OCEAN"),
       fill = c("#F8756D", "#00BE7D", "#A3A500", "#03B0F6", "#E76AF3"), 
       "topright")

"
There are different ways to treat categorical features:
  a) create n features instead of 1 and make them boolean (1hot);
  b) create the mapping function;
  c) etc..
Let's try to create the mapping function. The first prediction looking on the
map would be: INLAND -> <1H OCEAN -> NEAR BAY -> NEAR OCEAN -> ISLAND
But let's take a look at the medium house value for each category.
"

all_housing %>% ggplot(aes(x=ocean_proximity, y=median_house_value, 
                               fill=ocean_proximity)) +
  geom_boxplot() +
  scale_fill_viridis(discrete = TRUE, alpha=0.6) +
  geom_jitter(color="darkorange", size=0.4, alpha=0.4) +
  theme_ipsum() +
  theme(
    legend.position="none",
    plot.title = element_text(size=20),
    axis.text.x=element_text(size=12), 
    axis.text.y=element_text(size=12),
    axis.title.x=element_text(size=15),
    axis.title.y=element_text(size=15)
  ) +
  labs(title="Median House Value by the Ocean Proximity",
       x="Ocean Proximity", y='Median House Value')

"
So we see that our prediction was quite accurate. The exception to it is
that NEAR BAY and NEAR OCEAN are very close, but NEAR BAY median is bigger,
so we can flip them in our mapping. It can be caused because of the urbanization
of the bay coast. San Francisco, Berkley, Oakland, Richmond, Fremont, San Jose,
Silicon Valley, Sacramento is quite close to it. The whole coast is sprinkled
with big cities, so that's probably the reason of it.

We can also see how many outliers there are in the all proximity categories 
except ISLAND. It is because of the data, the problem is that all the median 
house values above 500k were inserted as 500k, so if the real distribution is X, 
we get Y ~ min(X, 500k), and it seems that it is gonna affect our analysis, 
so we must be aware of it, and potencially we could do some more data cleaning 
to get rid of those values, but it needs to be done very carefully, as we can 
affect the outcome very seriously.

The problem described above is telling us, that we went the right way by
comparing medians, not means, because Y's median is the same as the X's, but
mean is strongly affected. Median is always much more robust to the outliers.

By now, let's get back to Ocean Proximity and change its value using mapping:
INLAND -> 1, <1H OCEAN -> 2, NEAR OCEAN -> 3, NEAR BAY -> 4, ISLAND -> 5
"

fac <- factor(all_housing$ocean_proximity, 
       levels=c('INLAND', '<1H OCEAN', 'NEAR OCEAN', 'NEAR BAY', 'ISLAND'))
# let's check the mapping
data.frame(levels = unique(fac), value = as.numeric(unique(fac)))

all_housing$ocean_proximity <- as.numeric(fac)

# checking if the changes were made
str(all_housing)
head(all_housing)

# everything is ok, let's save our cleaned data to another csv file
write.csv(all_housing, file="cleaned_housing.csv", sep=",", 
          col.names=TRUE, row.names=FALSE)

lim <- max(all_housing$median_house_value)
lim
sum(all_housing$median_house_value == lim)

# cleaning all the variables created before
rm(list=ls())

# ---------------------------------------------------
# Cleaning finished. Let's start analyzing
library(corrplot)
library(purrr)
library(ggplot2)
library(hrbrthemes)
library(raster)
library(tidyverse)
library(viridis)
library(forcats)
library(dplyr)
library(ggpubr)
# libraries reloading
windowsFonts(
  A=windowsFont("Arial")
)

housing <- read.csv("cleaned_housing.csv", header=TRUE)
str(housing)
summary(housing)

# checking if the null values has left
colSums(is.na(housing))

corm <- cor(housing)
corrplot(corm, method='number')

# visualizing the distributions of each feature
density.plot.properties <- list(
  geom_density(fill="darkorange", color="darkorange", alpha=1),
  theme_ipsum_rc(grid='Y'),
  theme(
    legend.position="none",
    axis.title.x = element_blank(),
    axis.title.y = element_blank()
  ))
  
# longitude
pb1 <- housing %>%
  ggplot( aes(x=longitude)) + density.plot.properties

# latitude
pb2 <- housing %>%
  ggplot( aes(x=latitude)) + density.plot.properties

# housing_median_age
pb3 <- housing %>%
  ggplot( aes(x=housing_median_age)) + density.plot.properties

# total_rooms
pb4 <- housing %>%
  ggplot( aes(x=total_rooms)) + density.plot.properties

pb5 <- housing %>%
  filter(total_rooms < 10000) %>%
  ggplot( aes(x=total_rooms)) + density.plot.properties

# total_bedrooms
pb6 <- housing %>%
  ggplot( aes(x=total_bedrooms)) + density.plot.properties

pb7 <- housing %>%
  filter(total_bedrooms < 2000) %>%
  ggplot( aes(x=total_bedrooms)) + density.plot.properties

# population
pb8 <- housing %>%
  ggplot( aes(x=population)) + density.plot.properties

pb9 <- housing %>%
  filter(population < 5000) %>%
  ggplot( aes(x=population)) + density.plot.properties

# households
pb10 <- housing %>%
  ggplot( aes(x=households)) + density.plot.properties

pb11 <- housing %>%
  filter(households < 2000) %>%
  ggplot( aes(x=households)) + density.plot.properties

# median_income
pb12 <- housing %>%
  ggplot( aes(x=median_income)) + density.plot.properties

# median_house_value
pb13 <- housing %>%
  ggplot( aes(x=median_house_value)) + density.plot.properties

# ocean_proximity
pb14 <- housing %>%
  ggplot( aes(x=ocean_proximity)) + density.plot.properties

# drawing it all at one page
theme_set(theme_pubr())
ggarrange(pb1, pb2, pb3, pb4, pb5, pb6, pb7, pb8, pb9, pb10, pb11, pb12, pb13,
          pb14, ncol=4, nrow=4, font.label=list(size = 10, family='A'),
          labels=c('Longitude', 'Latitude', 'Median Age', 'Total Rooms',
                   'Total Rooms filtered (< 10k)', 'Total Bedrooms',
                   'Total Bedrooms filtered (< 2k)', 'Population',
                   'Population filtered (< 5k)', 'Households',
                   'Households filtered (< 2k)', 'Median Income',
                   'Median House Value', 'Ocean Proximity'))

"
As we see many of the features are right-tailed, so to effectively use it,
we should do something with them. Also we can see that they don't have the
best distributions, and the medium_house_value is very weakly correlated with
almost all the columns, so we should try to do something with it.
Let's try feature engineering!

Thinking of the features, we can imply that total_rooms, total_bedrooms don't
have any sense by only themselves. We should combine them with the other
features and see if the correlation grows.

So let's try out:
  bedrooms per rooms,
  rooms per household,
  bedrooms per person,
  rooms per person,
  bedrooms per household,
  people per household
  
We could have also tried to create features that consider distance to the
closest cities and their population and so on, but it would require lots of
analysis with the longitude and latitude, and that is not what we do want.
"

# creating new features
housing$bedrooms_per_rooms <- housing$total_bedrooms / housing$total_rooms
housing$rooms_per_household <- housing$total_rooms / housing$households
housing$bedrooms_per_person <- housing$total_bedrooms / housing$population
housing$rooms_per_person <- housing$total_rooms / housing$population
housing$bedrooms_per_household <- housing$total_bedrooms / housing$households
housing$people_per_household <- housing$population / housing$households

# checking the correlation
corfe <- cor(housing)
corrplot(corfe, method='number')

# bedrooms_per_rooms
pfe1 <- housing %>%
  ggplot( aes(x=bedrooms_per_rooms)) + density.plot.properties

# rooms_per_household
pfe2 <- housing %>%
  ggplot( aes(x=rooms_per_household)) + density.plot.properties

# bedrooms_per_person
pfe3 <- housing %>%
  ggplot( aes(x=bedrooms_per_person)) + density.plot.properties

# rooms_per_person
pfe4 <- housing %>%
  ggplot( aes(x=rooms_per_person)) + density.plot.properties

# people_per_household
pfe5 <- housing %>%
  ggplot( aes(x=people_per_household)) + density.plot.properties

# bedrooms_per_household
pfe6 <- housing %>%
  ggplot( aes(x=bedrooms_per_household)) + density.plot.properties

# drawing it all at one page
ggarrange(pfe1, pfe2, pfe3, pfe4, pfe5, pfe6, 
          ncol=2, nrow=3, font.label=list(size = 12, family='A'),
          labels=c('Bedrooms per room', 'Rooms per household',
                   'Bedrooms per person', 'Rooms per person',
                   'People per household', 'Bedrooms per household'))

# let's filter them to get the better scope
# bedrooms_per_rooms
pfe1f <- housing %>%
  filter(bedrooms_per_rooms < 0.4) %>%
  ggplot( aes(x=bedrooms_per_rooms)) + density.plot.properties

# rooms_per_household
pfe2f <- housing %>%
  filter(rooms_per_household < 11) %>%
  ggplot( aes(x=rooms_per_household)) + density.plot.properties

# bedrooms_per_person
pfe3f <- housing %>%
  filter(bedrooms_per_person < 1) %>%
  ggplot( aes(x=bedrooms_per_person)) + density.plot.properties

# rooms_per_person
pfe4f <- housing %>%
  filter(rooms_per_person < 4) %>%
  ggplot( aes(x=rooms_per_person)) + density.plot.properties

# people_per_household
pfe5f <- housing %>%
  filter(people_per_household < 6) %>%
  ggplot( aes(x=people_per_household)) + density.plot.properties

# bedrooms_per_household
pfe6f <- housing %>%
  filter(bedrooms_per_household < 2) %>%
  ggplot( aes(x=bedrooms_per_household)) + density.plot.properties

# drawing it all at one page
ggarrange(pfe1f, pfe2f, pfe3f, pfe4f, pfe5f, pfe6f,
          ncol=2, nrow=3, font.label=list(size = 12, family='A'),
          labels=c('Bedrooms per room filtered', 'Rooms per household filtered',
                   'Bedrooms per person filtered', 'Rooms per person filtered',
                   'People per household filtered', 
                   'Bedrooms per household filtered'))

"
As we can see bedrooms_per_person doesn't introduce anything new, and
bedrooms_per_room is much more better than total_bedrooms, so we replace
total_bedrooms with it.

The useful newly constructed feature for us would be rooms_per_person, as
the correlation between it and median_house_value is 0.21, so we
replace total_rooms with this new feature.

Another one not interesting in the terms of regression, but in the terms of the
correlation between other features and the distribution of itself is
people_per_household. As we know, we don't need strong correlation between
features based on which we predict the value of another one. So it helps us, 
because population was strongly correlated with the old features.

Let's see what a new dataset looks like
"

housingf <- dplyr::select(housing, -c('total_rooms', 'population', 'total_bedrooms',
                                      'bedrooms_per_household', 'rooms_per_household',
                                      'bedrooms_per_person'))
str(housingf)

fedone <- cor(housingf)
corrplot(fedone, method='number')

"
We did a great work! We don't have strong not-needed correlation except
longitude-latitude (that's because of the California shape), and
bedrooms_per_rooms - median-income, but that's not gonna affect the model
strognly. And, by the way, we got a few features which have much better
correlation with the median_house_value.

> 500k, scales, tail-heavy described in the report
"
write.csv(housingf, file="fe_housing.csv", sep=",", 
          col.names=TRUE, row.names=FALSE)

# cleaning all the variables created before
rm(list=ls())

# -----------------------------------------------
library(corrplot)
library(purrr)
library(ggplot2)
library(hrbrthemes)
library(raster)
library(tidyverse)
library(viridis)
library(forcats)
library(dplyr)
library(ggpubr)
library(psych)
library(moments)
library(reshape)
library(reshape2)
library(MASS)
library(pastecs)

# for future visualization
density.plot.properties <- list(
  geom_density(fill="darkorange", color="darkorange", alpha=1),
  theme_ipsum_rc(grid='XY', base_family = 'sans'),
  theme(
    legend.position="none",
    axis.title.x = element_blank(),
    axis.title.y = element_blank()
  ))

plot.properties <- list(
  theme_ipsum_rc(grid='XY', base_family = 'sans'),
  theme(
    legend.position="none",
    axis.title.x = element_blank(),
    axis.title.y = element_blank()
))

housing <- read.csv('fe_housing.csv', header=TRUE)

head(housing, 4)

summary(housing)

# all the decsriptive values (look at the skew and kurt, should see tail-heavy)
describeBy(housing)

# calculating all the moments
all.moments(housing)

# calculating all the central moments
all.moments(housing, central=TRUE)

# melting the data for convenient boxplotting
melted <- melt.data.frame(housing)
head(melted)

melted %>% ggplot(aes(x=variable, y=value, 
                           fill=variable)) +
  geom_boxplot() +
  scale_fill_viridis(discrete = TRUE, alpha=0.6) +
  geom_jitter(color="darkorange", size=0.05, alpha=0.1) +
  theme_ipsum() +
  facet_wrap(~variable,scales="free",ncol=5, nrow=2) +
  theme(
    legend.position="none",
    plot.title = element_text(size=20),
    axis.text.x=element_text(size=12), 
    axis.text.y=element_text(size=12),
    axis.title.x=element_blank(),
    axis.title.y=element_blank()
  ) +
  labs(title="Boxplots for each feature")


# replacing outliers with NAs to remove them while melting
outlier.replace <- function(x){
  quantiles <- quantile( x, c(.003, .997 ) )
  x[ x < quantiles[1] ] <- NA
  x[ x > quantiles[2] ] <- NA
  return(x)
}

no_outs <- housing
for(i in names(no_outs)){
  no_outs[[i]] <- outlier.replace(no_outs[[i]])
}

# outliers per column
colSums(is.na(no_outs))

# melting the 'no outliers' data
melted_outs <- melt.data.frame(no_outs, na.rm=TRUE)

melted_outs %>% ggplot(aes(x=variable, y=value, 
                      fill=variable)) +
  geom_boxplot() +
  scale_fill_viridis(discrete = TRUE, alpha=0.6) +
  geom_jitter(color="darkorange", size=0.05, alpha=0.1) +
  theme_ipsum() +
  facet_wrap(~variable,scales="free",ncol=5, nrow=2) +
  theme(
    legend.position="none",
    plot.title = element_text(size=20),
    axis.text.x=element_text(size=12), 
    axis.text.y=element_text(size=12),
    axis.title.x=element_blank(),
    axis.title.y=element_blank()
  ) +
  labs(title="Boxplots for each feature")

# density plots below
p1 <- no_outs %>% ggplot(aes(x=longitude)) + density.plot.properties
p2 <- no_outs %>% ggplot(aes(x=latitude)) + density.plot.properties
p3 <- no_outs %>% ggplot(aes(x=housing_median_age)) + density.plot.properties
p4 <- no_outs %>% ggplot(aes(x=households)) + density.plot.properties
p5 <- no_outs %>% ggplot(aes(x=median_income)) + density.plot.properties
p6 <- no_outs %>% ggplot(aes(x=median_house_value)) + density.plot.properties
p7 <- no_outs %>% ggplot(aes(x=ocean_proximity)) + density.plot.properties
p8 <- no_outs %>% ggplot(aes(x=bedrooms_per_rooms)) + density.plot.properties
p9 <- no_outs %>% ggplot(aes(x=rooms_per_person)) + density.plot.properties
p10 <- no_outs %>% ggplot(aes(x=people_per_household)) + density.plot.properties

# drawing it all at one page
ggarrange(p1, p2, p3, p4, p5, p6, p7, p8, p9, p10,
          ncol=2, nrow=5, font.label=list(size = 12, family='sans'),
          labels=names(housing))

# analysing the types of distributions
# bedrooms_per_rooms -------------------------------------
gap <- IQR(no_outs$bedrooms_per_rooms, na.rm=TRUE) / 
  (length(na.omit(no_outs$bedrooms_per_rooms)) ^ (1/3))

no_outs %>% ggplot(aes(x=bedrooms_per_rooms)) + plot.properties +
  geom_histogram(binwidth = gap) +
  labs(title='Bedrooms per rooms distribution')
# qqplot
no_outs %>% ggplot(aes(sample=bedrooms_per_rooms))+stat_qq() +
  labs(title='Bedrooms per rooms QQ-plot')
# qqplot for normal distribution
ggplot(data.frame(d = rnorm(1000)), aes(sample=d)) + stat_qq()

stat.desc(sample(no_outs$bedrooms_per_rooms, 150), norm=TRUE)

shapiro.test(sample(no_outs$bedrooms_per_rooms, 50))
shapiro.test(sample(no_outs$bedrooms_per_rooms, 50))
shapiro.test(sample(no_outs$bedrooms_per_rooms, 50))

# log10 distr (could be ln, but the difference is only the constant)
bprlog <- data.frame(d = log10(no_outs$bedrooms_per_rooms))

gap <- IQR(bprlog$d, na.rm=TRUE) / (length(na.omit(bprlog$d)) ^ (1/3))

bprlog %>% ggplot(aes(x=d)) + plot.properties +
  geom_histogram(binwidth = gap)
  labs(title='Bedrooms per rooms log10-distribution')
# qqplot
bprlog %>% ggplot(aes(sample=d))+stat_qq() +
  labs(title='Bedrooms per rooms log10-distribution QQ-plot')

stat.desc(sample(bprlog$d, 150), norm=TRUE)

shapiro.test(sample(bprlog$d, 50))
shapiro.test(sample(bprlog$d, 50))
shapiro.test(sample(bprlog$d, 50))

# it is non-normal distribution

# median_income ------------------------------------------
gap <- IQR(no_outs$median_income, na.rm=TRUE) / 
  (length(na.omit(no_outs$median_income)) ^ (1/3))

no_outs %>% ggplot(aes(x=median_income)) + plot.properties +
  geom_histogram(binwidth = gap) +
  labs(title='Median Income distribution')
# qqplot
no_outs %>% ggplot(aes(sample=median_income))+stat_qq() +
  labs(title='Median Income QQ-plot')

shapiro.test(sample(no_outs$median_income, 2000))
shapiro.test(sample(no_outs$median_income, 100))
shapiro.test(sample(no_outs$median_income, 200))

# log10 distr (could be ln, but the difference is only the constant)
milog <- data.frame(d = log10(no_outs$median_income))

gap <- IQR(milog$d, na.rm=TRUE) / (length(na.omit(milog$d)) ^ (1/3))

milog %>% ggplot(aes(x=d)) + plot.properties +
  geom_histogram(binwidth = gap) +
  labs(title='Median Income log10-distribution')
# qqplot
milog %>% ggplot(aes(sample=d))+stat_qq() +
  labs(title='Median Income log10-distribution QQ-plot')

stat.desc(sample(milog$d, 150), norm=TRUE)

shapiro.test(sample(milog$d, 50))
shapiro.test(sample(milog$d, 50))
shapiro.test(sample(milog$d, 50))

# it is non-normal distribution, although the log10 distr can be described so

# median_house_value -------------------------------------
gap <- IQR(no_outs$median_house_value, na.rm=TRUE) / 
  (length(na.omit(no_outs$median_house_value)) ^ (1/3))

no_outs %>% ggplot(aes(x=median_house_value)) + plot.properties +
  geom_histogram(binwidth = gap) +
  labs(title='Median House Value distribution')
# qqplot
no_outs %>% ggplot(aes(sample=median_house_value))+stat_qq() +
  labs(title='Median House Value QQ-plot')

shapiro.test(sample(no_outs$median_house_value, 2000))
shapiro.test(sample(no_outs$median_house_value, 100))
shapiro.test(sample(no_outs$median_house_value, 200))

# log10 distr (could be ln, but the difference is only the constant)
mhvlog <- data.frame(d = log10(no_outs$median_house_value))

mhvlog <- mhvlog %>% filter(d < 5.698)

gap <- IQR(mhvlog$d, na.rm=TRUE) / (length(na.omit(mhvlog$d)) ^ (1/3))

mhvlog %>% ggplot(aes(x=d)) + plot.properties +
  geom_histogram(binwidth = gap) +
  labs(title='Median House Value log10-distribution')
# qqplot
mhvlog %>% ggplot(aes(sample=d))+stat_qq() +
  labs(title='Median House Value log10-distribution QQ-plot')

shapiro.test(sample(mhvlog$d, 50))
shapiro.test(sample(mhvlog$d, 50))
shapiro.test(sample(mhvlog$d, 50))

# point and interval estimation of 
# median_income, median_house_value and bedrooms_per_rooms

# !!! the code is not absolutely valid here, use the one from .rnw file

estimations <- data.frame(mean=double(),
                          meanlow=double(),
                          meanup=double(),
                          var=double(),
                          varlow=double(),
                          varup=double()
                          )


income = na.omit(no_outs$median_income)
n1 = length(income) 
s1 = sd(income)
SE1 = s1/sqrt(n1)
E1 = qt(.975, df = n1 - 1) * SE1
mu1 = mean(income)
var1 = var(income)
newr1 <- data.frame(mu1, NA, NA, var1, NA, NA)
names(newr1) <- names(estimations)
estimations <- rbind(estimations, newr1)

house.value = na.omit(no_outs$median_house_value)
n2 = length(house.value) 
s2 = sd(house.value)
SE2 = s2/sqrt(n2)
E2 = qt(.975, df = n2 - 1) * SE2
mu2 = mean(house.value)
var2 = var(house.value)
newr2 <- data.frame(mu2, NA, NA, var2, NA, NA)
names(newr2) <- names(estimations)
estimations <- rbind(estimations, newr2)

# var2*(n2-1)/qchisq(.975, n2-1), 
# var2*(n2-1)/qchisq(.025, n2-1)

# mu3 - E3, mu3 + E3

bpr = na.omit(no_outs$bedrooms_per_rooms)
n3 = length(bpr) 
s3 = sd(bpr)
SE3 = s3/sqrt(n3)
E3 = qt(.975, df = n3 - 1) * SE3
mu3 = mean(bpr)
var3 = var(bpr)
newr3 <- data.frame(mu3, NA, NA, var3, NA, NA)
names(newr3) <- names(estimations)
estimations <- rbind(estimations, newr3)

# 1 - median_income
# 2 - median_house_value
# 3 - bedrooms_per_room
estimations

# testing different hypotheses
# let's look at the correlation matrix to try to ask some questions
Mcor <- cor(housing)
corrplot(Mcor, method='number')

# how does ocean_proximity affect median_house_value? ANOVA / Kruskal-Wallis
krusktmpdata <- no_outs %>% filter(ocean_proximity != 5)

kruskal.test(median_house_value ~ ocean_proximity, data = krusktmpdata)

# how being 'near bay' and 'near ocean' affect median price?
whitntmpdata <- no_outs %>% filter(ocean_proximity == 3 || ocean_proximity == 4)

wilcox.test(whitntmpdata$median_house_value, whitntmpdata$ocean_proximity)

pairwise.wilcox.test(krusktmpdata$median_house_value, 
                     krusktmpdata$ocean_proximity,
                     p.adjust.method = "BH")

# how does median_income affect median_house_value? regression + correlation test
cor.test(no_outs$median_house_value, no_outs$median_income, method = "spearman")

# how does rooms_per_person affect median_house_value? correlation test
cor.test(no_outs$median_house_value, no_outs$rooms_per_person, method = "spearman")

# regression model
model <- lm(median_house_value ~ median_income + ocean_proximity +
              bedrooms_per_rooms + rooms_per_person + latitude + 
              housing_median_age, no_outs, na.action=na.omit)
summary(model)

gap <- IQR(model$residuals, na.rm=TRUE) / 
  (length(na.omit(model$residuals)) ^ (1/3))

model %>% ggplot(aes(x=model$residuals)) + plot.properties +
  geom_histogram(binwidth = gap) +
  labs(title='Model residuals distribution')

mean(model$residuals)

cor(model$model, model$residuals)

l <- length(model$residuals)
model %>% ggplot(aes(x=1:l, y=model$residuals)) + plot.properties +
  geom_point() +
  labs(title='Model Residuals')

varcor <- cor(model$model)
corrplot(varcor, method='number')

model %>% ggplot(aes(sample=model$residuals))+stat_qq() +
  labs(title='Model Residuals QQ-plot')

shapiro.test(sample(model$residuals, 50))
shapiro.test(sample(model$residuals, 50))
shapiro.test(sample(model$residuals, 50))
shapiro.test(sample(model$residuals, 50))
shapiro.test(sample(model$residuals, 50))

