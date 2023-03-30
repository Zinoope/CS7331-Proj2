### PART I: cleaning and Visualization ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

## Step I-01: Loading the Dataset ----------------------------------------------------
#load the csv file/ check how many missing values do we have
library("tidyverse")
library("ggrepel")
library("DT")
library("NbClust")
library("dbscan")
library("seriation")
cases <- read_csv("../data/Ohio Covid 03-05 + Census 2020-5yrs + Geo Boundaries.csv")
cases <- cases %>% mutate_if(is.character, factor)
cases
# check for NA values
is.na(cases) %>% sum()

## Step I-02: Aggregation, Normalization and Selection -----------------------------
cases_filtered <- cases %>% mutate(
  female_under_40_ratio= (female_under_5 +
                            female_5_to_9 +
                            female_10_to_14 +
                            female_15_to_17 +
                            female_18_to_19 +
                            female_20 +
                            female_21 +
                            female_22_to_24 +
                            female_25_to_29 +
                            female_30_to_34 +
                            female_35_to_39) / total_pop,
  
  female_pop_P100=female_pop/total_pop*100,
  
  male_under_40_ratio= (male_under_5 +
                          male_5_to_9 +
                          male_10_to_14 +
                          male_15_to_17 +
                          male_18_to_19 +
                          male_20 +
                          male_21 +
                          male_22_to_24 +
                          male_25_to_29 +
                          male_30_to_34 +
                          male_35_to_39) / total_pop,
  
  male_pop_P100=male_pop/total_pop*100,
  
  asian_pop_P1000=asian_pop/total_pop*1000,
  black_pop_P1000=black_pop/total_pop*1000,
  hispanic_pop_P1000=hispanic_pop/total_pop*1000,
  
  deaths_P1000 = deaths/total_pop*1000,
  confirmed_cases_P1000= confirmed_cases/total_pop*1000,
  
  walked_to_work_P1000 = walked_to_work/total_pop*1000,
  commuters_by_public_transportation_P1000 = commuters_by_public_transportation/total_pop*1000,
  commuters_by_carpool_P1000 = commuters_by_carpool/total_pop*1000,
  commuters_drove_alone_P1000 = commuters_drove_alone/total_pop*1000,
  
  pop_density_Pkm= total_pop * 10^6 /area_land_meters
  
)

cases_cleaned <- cases_filtered %>% select(county_name,
                                           # Ground truth
                                           confirmed_cases_P1000,
                                           deaths_P1000,
                                           # first Subset
                                              # gender/Ethnicity and age
                                           female_pop_P100,
                                           male_pop_P100,
                                           female_under_40_ratio,
                                           male_under_40_ratio,
                                              # habits and interactions
                                           walked_to_work_P1000,
                                           commuters_by_public_transportation_P1000,
                                           commuters_by_carpool_P1000,
                                           commuters_drove_alone_P1000,
                                              # financial related
                                           income_per_capita,
                                           
                                           # Second Subset
                                              # gender/Ethnicity and age
                                           asian_pop_P1000,
                                           black_pop_P1000,
                                           hispanic_pop_P1000,
                                           median_age,
                                              # habits and interactions
                                           pop_density_Pkm,
                                              # financial related
                                           median_income,
                                           
                                           
) 

# check for NA values
is.na(cases_cleaned) %>% sum()
cases_cleaned
rm(cases, cases_filtered)

## Step I-03:  Data visualization [deaths and detected maps] -----------------------
counties <- as_tibble(map_data("county"))
counties_OH <- counties %>% dplyr::filter(region == "ohio") %>% 
  rename(c(county = subregion))
cases_OH <- cases_cleaned %>% mutate(county = county_name %>% 
                                  str_to_lower() %>% str_replace('\\s+county\\s*$', ''))

counties_OH_clust <- counties_OH %>% left_join(cases_OH)
rm(counties,counties_OH,cases_OH)


deaths_map <- ggplot(counties_OH_clust, aes(long, lat)) + 
  geom_polygon(aes(group = group, fill = deaths_P1000)) +
  coord_quickmap() +
  scale_fill_continuous(type = "viridis") +
  labs(title = "Deaths in Ohio State", fill = "Deaths per 1000")
cases_map <- ggplot(counties_OH_clust, aes(long, lat)) + 
  geom_polygon(aes(group = group, fill = confirmed_cases_P1000)) +
  coord_quickmap() +
  scale_fill_continuous(type = "viridis") +
  labs(title = "Cases in Ohio State", fill = "Cases per 1000")

cowplot::plot_grid(deaths_map, cases_map, nrow = 1, ncol = 2)
rm(counties_OH_clust, cases_map, deaths_map)

### PART II: clustering ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

## Step II-01:  Prepare Clusters ----------------------------------------------------

# 1. select attributes and scale the values to Z-scores ^^^^^^^^^^^^^^^^^^^^^^^^^^^^^

subset01_to_cluster <- subset(cases_cleaned, select = ( c(
    # gender/Ethnicity and age
  female_pop_P100,
  male_pop_P100,
  female_under_40_ratio,
  male_under_40_ratio,
    # habits and interactions
  walked_to_work_P1000,
  commuters_by_public_transportation_P1000,
  commuters_by_carpool_P1000,
  commuters_drove_alone_P1000,
    # financial related
  income_per_capita
  ))) %>% scale() %>% as_tibble()
subset01_to_cluster <- subset01_to_cluster %>% add_column(county_name = cases_cleaned$county_name) 
summary(subset01_to_cluster)  

subset02_to_cluster <- subset(cases_cleaned, select = ( c(
    # gender/Ethnicity and age
  asian_pop_P1000,
  black_pop_P1000,
  hispanic_pop_P1000,
  median_age,
    # habits and interactions
  pop_density_Pkm,
    # financial related
  median_income
))) %>% scale() %>% as_tibble()
subset02_to_cluster <- subset02_to_cluster %>% add_column(county_name = cases_cleaned$county_name)
summary(subset02_to_cluster)

# 2. perform a PCA analysis and remove outliers ^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^ 
# a- subset 01:
PCA <- subset01_to_cluster %>% select(-county_name) %>% prcomp()

PCA <- as_tibble(PCA$x)  %>% add_column(county_name = cases_cleaned$county_name)

lof <- lof(as_tibble(PCA$PC1,PCA$PC2), minPts= 10)
ggplot(PCA %>% add_column(lof = lof), aes(PC1, PC2, color = lof)) +
  geom_point() + scale_color_gradient(low = "gray", high = "red")+
  geom_text_repel(aes(label = county_name), vjust = -1)+
  labs(title = "PCA outlier analysis", subtitle="Subset 01", fill = "Local Outlier Factor")

ggplot(PCA %>% add_column(outlier = lof >= 5), aes(PC1, PC2, color = outlier)) +
  geom_point()+
  geom_text_repel(aes(label = county_name), vjust = -1)

subset01_to_cluster <- subset01_to_cluster %>% filter(lof < 5) 

# b- subset 02:
PCA <- subset02_to_cluster %>% select(-county_name) %>% prcomp()

PCA <- as_tibble(PCA$x)  %>% add_column(county_name = cases_cleaned$county_name)

lof <- lof(as_tibble(PCA$PC1,PCA$PC2), minPts= 10)
ggplot(PCA %>% add_column(lof = lof), aes(PC1, PC2, color = lof)) +
  geom_point() + scale_color_gradient(low = "gray", high = "red")+
  geom_text_repel(aes(label = county_name), vjust = -1)

ggplot(PCA %>% add_column(outlier = lof >= 1.75), aes(PC1, PC2, color = outlier)) +
  geom_point()+
  geom_text_repel(aes(label = county_name), vjust = -1)

subset02_to_cluster <- subset02_to_cluster %>% filter(lof < 1.75) 


rm(PCA, lof)

## Step II-02:  K-Means -------------------------------------------------------------

# 1- find the number of clusters ^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^
# A- Subset 01:
# A-1-1 Elbow Method: Within-Cluster Sum of Squares
set.seed(1234)
ks <- 2:10

WCSS <- sapply(ks, FUN = function(k) {
  kmeans(subset01_to_cluster %>% select(-county_name), centers = k, nstart = 1000)$tot.withinss
})

WCSS_viz <- ggplot(as_tibble(ks, WCSS), aes(ks, WCSS)) + geom_line() +
  geom_vline(xintercept = 8, color = "red", linetype = 2)+
  geom_vline(xintercept = 7, color = "blue", linetype = 2)

# A-1-2 Average Silhouette Width
d <- dist(subset01_to_cluster %>% select(-county_name))


ASW <- sapply(ks, FUN=function(k) {
  fpc::cluster.stats(d, kmeans(subset01_to_cluster %>% select(-county_name), centers=k, nstart = 1000)$cluster)$avg.silwidth
})

ASW_viz <- ggplot(as_tibble(ks, ASW), aes(ks, ASW)) + geom_line() +
  geom_vline(xintercept = 8, color = "red", linetype = 2)+
  geom_vline(xintercept = 7, color = "blue", linetype = 2)

cowplot::plot_grid(WCSS_viz, ASW_viz, nrow = 1, ncol = 2)

k01=7
km01=kmeans(subset01_to_cluster %>% select(-county_name), centers = k01, nstart = 1000)

# B- Subset 02:
# B-1-1 Elbow Method: Within-Cluster Sum of Squares
set.seed(1234)
ks <- 2:20

WCSS <- sapply(ks, FUN = function(k) {
  kmeans(subset02_to_cluster %>% select(-county_name), centers = k, nstart = 1000)$tot.withinss
})

WCSS_viz <- ggplot(as_tibble(ks, WCSS), aes(ks, WCSS)) + geom_line() 

# A-1-2 Average Silhouette Width
d <- dist(subset02_to_cluster %>% select(-county_name))


ASW <- sapply(ks, FUN=function(k) {
  fpc::cluster.stats(d, kmeans(subset02_to_cluster %>% select(-county_name), centers=k, nstart = 1000)$cluster)$avg.silwidth
})

ASW_viz <- ggplot(as_tibble(ks, ASW), aes(ks, ASW)) + geom_line()


cowplot::plot_grid(WCSS_viz, ASW_viz, nrow = 1, ncol = 2)

k02=9
km02=kmeans(subset02_to_cluster %>% select(-county_name), centers = k02, nstart = 1000)

rm(ASW,ASW_viz,WCSS,WCSS_viz,d,ks)

#NbClust(data = subset02_to_cluster, diss = NULL, distance = "euclidean", min.nc = 2, max.nc = 15,
#        method = "kmeans", index = "all", alphaBeale = 0.1)

# 2. displaying details of the cluster ^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^

ggplot(pivot_longer(as_tibble(km01$centers,  rownames = "cluster"), 
                    cols = colnames(km01$centers)), 
                    aes(y = name, x = value)) +
                    geom_bar(stat = "identity") +
                    facet_grid(rows = vars(cluster))
ggplot(pivot_longer(as_tibble(km02$centers,  rownames = "cluster"), 
                    cols = colnames(km02$centers)), 
                    aes(y = name, x = value)) +
                    geom_bar(stat = "identity") +
                    facet_grid(rows = vars(cluster))

# visualize the clusters in small adjacent maps:

counties <- as_tibble(map_data("county"))
counties_OH <- counties %>% dplyr::filter(region == "ohio") %>% 
  rename(c(county = subregion))

#a. first subset

cases_OH <- subset01_to_cluster %>% mutate(county = county_name %>% 
                                  str_to_lower() %>% str_replace('\\s+county\\s*$', ''))

counties_OH_clust <- counties_OH %>% left_join(cases_OH %>% 
                                                 add_column(cluster = factor(km01$cluster)))
km01_viz <- ggplot(counties_OH_clust, aes(long, lat)) + 
  geom_polygon(aes(group = group, fill = cluster)) +
  coord_quickmap() + 
  scale_fill_viridis_d() + 
  theme_minimal()+
  labs(title = "Clusters", subtitle = "Kmeans [Subset 01]")

#b. second subset

cases_OH <- subset02_to_cluster %>% mutate(county = county_name %>% 
                                             str_to_lower() %>% str_replace('\\s+county\\s*$', ''))

counties_OH_clust <- counties_OH %>% left_join(cases_OH %>% 
                                                 add_column(cluster = factor(km02$cluster)))
km02_viz <- ggplot(counties_OH_clust, aes(long, lat)) + 
  geom_polygon(aes(group = group, fill = cluster)) +
  coord_quickmap() + 
  scale_fill_viridis_d() + 
  theme_minimal()+
  labs(title = "Clusters", subtitle = "Kmeans [Subset 02]")

cowplot::plot_grid(km01_viz, km02_viz, nrow = 1, ncol = 2)


# 3- performing some internal validation visualizations:
d<- dist(subset01_to_cluster %>% select(-county_name))
pimage(d, order=order(km01$cluster), col = bluered(100))
dissplot(d, labels = km01$cluster, options=list(main="k-means with k=7"))

d<- dist(subset02_to_cluster %>% select(-county_name))
pimage(d, order=order(km02$cluster), col = bluered(100))
dissplot(d, labels = km02$cluster, options=list(main="k-means with k=9"))

## Step II-03:  Hierarchical --------------------------------------------------------







