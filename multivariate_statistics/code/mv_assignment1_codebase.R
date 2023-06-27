##
#
# Author: Julian Albert
# Date: 01 October 2019
#
# Description:
# MV Assignment 
#
#------------------------------------------------------------------------------#

# 0. Clean Workspace, Directory and Library ----

## Clean Workspace
rm(list=ls())
dev.off() # Close Plots
setwd("~") # Clear Path to User

## Locations
project_folder <- "/Documents/UCT/Coursework/MV"
loc_script <- "/Assignment_1/MSc_MV_Assignment"
loc_figs <- "/Assignment_1/Figs"
loc_data <- "/Assignment_1/MSc_MV_Assignment"

## Directories
dir_script <- paste("~", project_folder, loc_script, sep = '')
dir_figs <- paste("~", project_folder, loc_figs, sep = '')
dir_data <- paste("~", project_folder, loc_data, sep = '')
## Filename
dat_filen <- "/SpotifyFeatures.csv"

## Set Working Directory to Script Location
setwd(dir_script)

## Libraries - Lazyload
if (!require("pacman")) install.packages("pacman") 
p_load(tidyverse,fpc, data.table, Cairo, factoextra, cluster, 
       xtable, foreign, stargazer, scatterplot3d, Rtsne, wesanderson)

colour_vec <- c("dodgerblue3", "firebrick2", "forestgreen", "gold", 
                "darkorange", "darkorchid3") # colour for pretty plot

set.seed(420)
spotify_data <- read.csv(file = "SpotifyFeatures.csv")

## Just going to pretend that children's music isn't a thing for plots
spotify_data <- spotify_data %>%
  filter(genre != "Children's Music" & genre != "Children’s Music")
spotify_data$genre <- factor(spotify_data$genre) ## reset factors

# This shows the freq of genres (All pretty similar) 
# table(spotify_data[,1])
test <- spotify_data %>%
  filter(genre == "Opera" | genre == "Pop" | genre == "Jazz" | 
           genre == "Indie"| genre == "Comedy") 

cols_selected_vars <- ifelse(levels(spotify_data$genre) == "Opera" | 
                               levels(spotify_data$genre) == "Pop" | 
                               levels(spotify_data$genre) == "Jazz" | 
                               levels(spotify_data$genre) == "Indie"| 
                               levels(spotify_data$genre) == "Comedy", 
                             colour_vec[1], 'grey') 

setwd(dir_figs)
cairo_pdf("MV_Ass_fig_EDA_boxplots.pdf", height = 5, width = 7.5)
layout(matrix(c(1,1,2,2), ncol=2, byrow=TRUE), heights=c(4, 1))
par(mai = rep(0.5, 4))
boxplot(spotify_data$popularity ~ spotify_data$genre, col = cols_selected_vars , 
        ylab = "Popularity", xlab = "Genre")
par(mai=c(0,0,0,0))
plot.new()
legend(x = "center", ncol = 5, legend = c('Comedy', 'Indie', 'Jazz', 'Opera', 'Pop'),
       fill = rep(colour_vec[1], 5), x.intersp = 0.2,
       title = expression(paste(bold("Genre Selected (left to right)"))))
dev.off()

filtered_spotify_data <- spotify_data %>% 
  filter(genre == "Opera" | genre == "Pop" | genre == "Jazz" | genre == "Indie"| genre == "Comedy")  
filtered_spotify_data$genre <- factor(filtered_spotify_data$genre)

# We now sample only 1000 songs 
small_filtered_spotify_data <- sample_n(filtered_spotify_data, 1000, replace = FALSE)

freq_genre <- rbind(table(filtered_spotify_data$genre),
                    table(small_filtered_spotify_data$genre)) 

rownames(freq_genre) <- c("Full", "Sampled")
setwd(dir_figs)
print(xtable(freq_genre, type = "latex"), file = "MV_Ass_freq_mat.tex")

# Splits popularity into groups of 3
get_popularity <- function(x) {
  values <- c(66,33)
  if (x > values[1]) { return("Popular") }
  else if (x > values[2] & x < values[1]) { return("Mediocore") }
  else { return("Unpopular") }
}

# Didnt know how to do this in applys sorry :(
for(i in 1:NROW(small_filtered_spotify_data)){
  small_filtered_spotify_data$popularity_score[i] <- get_popularity(small_filtered_spotify_data$popularity[i])
}


dat <- small_filtered_spotify_data %>% 
  select(-c(genre,  # This will be added on at the end
            popularity_score, # This will be added on at the end
            popularity, 
            artist_name,
            track_name,
            track_id,
            key,
            mode,
            time_signature,
            liveness,
            duration_ms,
            tempo
  ))

# All have some form of correlation
cor_mat <- round(cor(dat),3) # TODO:need to tabulate 

## Save table in current directory to a .tex file
print(xtable(cor_mat, type = "latex"), file = "MV_Ass_corr_mat.tex")

prin_comp <- prcomp(dat, scale. = TRUE) #we normalize the variables to have standard deviation equals to 1.

setwd(dir_figs)
cairo_pdf("MV_Ass_fig_biplot.pdf", height = 5, width = 5)
fviz_pca_biplot(prin_comp, geom="point", col.var = colour_vec[2]) +
  theme_bw()
dev.off()

pr_var <- prin_comp$sdev^2
prop_varex <- pr_var/sum(pr_var)

setwd(dir_figs)
cairo_pdf("MV_Ass_fig_prop_varexplained.pdf", height = 5, width = 5)
plot(prop_varex, xlab = "Principal Component", 
     ylab = "Proportion of Variance Explained", type = "o")
points(prop_varex, pch = 19)
dev.off()

setwd(dir_figs)
cairo_pdf("MV_Ass_fig_cumprop_varexplained.pdf", height = 5, width = 5)
plot(cumsum( prin_comp$sdev^2)/sum( prin_comp$sdev^2), type = 'o',
     xlab = "No. of Principal Components", ylab = "Cumulative Proportion of Variance Explained")
points(cumsum( prin_comp$sdev^2)/sum( prin_comp$sdev^2), pch = 19)
dev.off()

# PCA 
plotting_df <- as.matrix(dat) %*% prin_comp$rotation[, 1:3] %>% 
  cbind(genre = small_filtered_spotify_data$genre,
        popularity = small_filtered_spotify_data$popularity_score) %>% 
  as.data.frame() %>%
  rename(Popularity = popularity)
plotting_df$PC1 <- as.numeric(paste(plotting_df$PC1))
plotting_df$PC2 <- as.numeric(paste(plotting_df$PC2))
plotting_df$PC3 <- as.numeric(paste(plotting_df$PC3))
plotting_df$genre <- as.factor(plotting_df$genre)
plotting_df$popularity <- as.factor(plotting_df$Popularity)

# 2 D
setwd(dir_figs)
cairo_pdf("MV_Ass_fig_1st2PCs.pdf", height = 5, width = 5)
plotting_df %>% 
  ggplot(aes(x = PC1, y = PC2, color = Popularity) )+
  geom_point() +
  scale_color_manual(values=colour_vec[1:3]) +
  theme_bw()
dev.off()

# 3 D
shapes_3d = c(16, 17, 18) 
cols_3d = colour_vec[1:3]
shapes_3d <- shapes_3d[as.numeric(plotting_df$Popularity)]
cols_3d <- cols_3d[as.numeric(plotting_df$Popularity)]

cairo_pdf("MV_Ass_fig_1st3PCs.pdf", height = 5, width = 5)
scatterplot3d(plotting_df[, 1:3], pch = shapes_3d, color = cols_3d,
              angle = -75) # type = 'h' # make lines to plane
dev.off()

# T-SNE
tsne_dat <- dat
# scale the data
tsne_dat <- apply(tsne_dat, MARGIN = 2, scale) %>% unique()
# dim = output dimensions
# 3*Perplexity !> NROW(X) -1
# This value effectively controls how many nearest neighbours are taken into account
# when constructing the embedding in the low-dimensional space.
# Theta Speed/accuracy trade-off (increase for less accuracy), set to 0.0 for exact TSNE
tsne_out <- Rtsne(tsne_dat, 
                  dim = 3, 
                  pca = FALSE, 
                  perplexity = 300,
                  theta = 0.0, 
                  normalize = TRUE)

test_dataframe = as.data.frame(tsne_out$Y)
test_dataframe$Popularity = as.factor(small_filtered_spotify_data$popularity_score)

# 2 D
cairo_pdf("MV_Ass_fig_1st2tSNEs.pdf", height = 5, width = 5)
test_dataframe %>% 
  ggplot(aes(x = V1, y = V2, color = Popularity) )+
  geom_point() +
  scale_color_manual(values=colour_vec[1:3]) +
  theme_bw()
dev.off()

cairo_pdf("MV_Ass_fig_1st3tSNEs.pdf", height = 5, width = 5)
scatterplot3d(test_dataframe[, 1:3], pch = shapes_3d, color = cols_3d,
              angle = 165) # type = 'h' # make lines to plane
dev.off()

setwd(dir_figs)
# Kmeans ---- 

# Elbow method
cairo_pdf("MV_Ass_fig_OptClusters.pdf", height = 5, width = 5)
fviz_nbclust((as.matrix(dat) %*% prin_comp$rotation[, 1:3]), kmeans, method = "wss") +
  geom_vline(xintercept = 3, linetype = 2)+
  labs(subtitle = "Elbow method") + 
  theme_bw()
dev.off()

# same same just longer computationally
# fviz_nbclust((as.matrix(dat) %*% prin_comp$rotation[, 1:3]), cluster::fanny, method = "wss") +
#   geom_vline(xintercept = 3, linetype = 2)+
#   labs(subtitle = "Elbow method") + 
#   theme_bw()

## Kmeans PCA
res.kmeans.PCA <- as.matrix(dat) %*% prin_comp$rotation[,1:3] %>% kmeans(3)
cairo_pdf("MV_Ass_fig_res_kmeans_PCA.pdf", height = 5, width = 5)
res.kmeans.PCA %>% 
  fviz_cluster(as.matrix(dat) %*% prin_comp$rotation[,1:3],ellipse.type = "norm",
               palette = wes_palette("Darjeeling1"), labelsize = 0, pointsize = 1) +
  theme_bw() + 
  labs(title = "Cluster Assignment",
       subtitle = "K-means applied to PCA Reduced Datasets")
dev.off()

## Kmeans Tsne
res.kmeans.tsne <- test_dataframe[,1:3] %>% kmeans(3)
cairo_pdf("MV_Ass_fig_res_kmeans_tsne.pdf", height = 5, width = 5)
res.kmeans.tsne %>% 
  fviz_cluster(test_dataframe[,1:3],ellipse.type = "norm",
               palette = wes_palette("Darjeeling1"), labelsize = 0, pointsize = 1) +
theme_bw() + 
  labs(title = "Cluster Assignment",
       subtitle = "K-means applied to t-SNE Reduced Datasets")
dev.off()


## Fuzzy ----

# Fuzzy PCA

res.fuzzy.PCA <- as.matrix(dat) %*% prin_comp$rotation[, 1:3] %>% fanny(3)
cairo_pdf("MV_Ass_fig_res_fuzzy_PCA.pdf", height = 5, width = 5)
res.fuzzy.PCA %>% 
  fviz_cluster(as.matrix(dat) %*% prin_comp$rotation[, 1:3], ellipse.type = "norm",
               palette = wes_palette("Darjeeling1"), labelsize = 0, pointsize = 1) +
theme_bw() + 
  labs(title = "Cluster Assignment",
       subtitle = "Fuzzy applied to PCA Reduced Datasets")
dev.off()


# Fuzzy TSNE

res.fuzzy.tsne <- test_dataframe[,1:3] %>% fanny(3)
cairo_pdf("MV_Ass_fig_res_fuzzy_tsne.pdf", height = 5, width = 5)
res.fuzzy.tsne %>% 
  fviz_cluster(test_dataframe[, 1:3], ellipse.type = "norm",
               palette = wes_palette("Darjeeling1"), labelsize = 0, pointsize = 1) +
  theme_bw() + 
  labs(title = "Cluster Assignment",
       subtitle = "Fuzzy applied to t-SNE Reduced Dataset")
dev.off()

res.fuzzy.PCA$silinfo
res.fuzzy.tsne$silinfo

setwd(dir_figs)
cairo_pdf("MV_Ass_fig_sil_fuzzy_PCA.pdf", height = 5, width = 5)
fviz_silhouette(res.fuzzy.PCA, palette = wes_palette("Darjeeling1")) +
  theme_bw() +
  theme(axis.text.x = element_blank(), axis.ticks.x = element_blank()) +
  labs(y = expression("Silhouette Width"~"("~S[i]~")"),
       title = "Cluster Silhouette Plot",
       subtitle = "Average width: 0.48")
dev.off()

cairo_pdf("MV_Ass_fig_sil_fuzzy_tsne.pdf", height = 5, width = 5)
fviz_silhouette(res.fuzzy.tsne, palette = wes_palette("Darjeeling1")) +
  theme_bw() +
  theme(axis.text.x = element_blank(), axis.ticks.x = element_blank()) +
  labs(y = expression("Silhouette Width"~"("~S[i]~")"),
       title = "Cluster Silhouette Plot",
       subtitle = "Average width: 0.55")
dev.off()

dd_PCA <- dist((as.matrix(dat) %*% prin_comp$rotation[,1:3]), method ="euclidean")
dd_tsne <- dist(test_dataframe[,1:3], method ="euclidean")

cairo_pdf("MV_Ass_fig_sil_kmeans_PCA.pdf", height = 5, width = 5)
res.kmeans.PCA$cluster %>%
  silhouette(dd_PCA) %>%
  fviz_silhouette(palette = wes_palette("Darjeeling1")) +
  theme_bw() +
  theme(axis.text.x = element_blank(), axis.ticks.x = element_blank()) +
  labs(y = expression("Silhouette Width"~"("~S[i]~")"),
       title = "Cluster Silhouette Plot",
       subtitle = "Average width: 0.55")
dev.off()

cairo_pdf("MV_Ass_fig_sil_kmeans_tsne.pdf", height = 5, width = 5)
res.kmeans.tsne$cluster %>%
  silhouette(dd_tsne) %>%
  fviz_silhouette(palette = wes_palette("Darjeeling1")) +
  theme_bw() +
  theme(axis.text.x = element_blank(), axis.ticks.x = element_blank()) +
  labs(y = expression("Silhouette Width"~"("~S[i]~")"),
       title = "Cluster Silhouette Plot",
       subtitle = "Average width: 0.56")
dev.off()

stats_kmeans_PCA <- cluster.stats(dd_PCA, res.kmeans.PCA$cluster)
stats_kmeans_tsne <- cluster.stats(dd_tsne, res.kmeans.tsne$cluster)
stats_fuzzy_PCA <- cluster.stats(dd_PCA, res.fuzzy.PCA$cluster)
stats_fuzzy_tsne <- cluster.stats(dd_tsne, res.fuzzy.tsne$cluster)

print(xtable(corrected_rand_table), type = "latex", file = "MV_Ass_corrected_rand_tab.tex")

print(xtable(
cbind(table(small_filtered_spotify_data$popularity_score, res.kmeans.PCA$cluster),
table(small_filtered_spotify_data$popularity_score, res.fuzzy.PCA$cluster))),
type = "latex", file = "MV_Ass_ext_PCAval_tab.tex")

print(xtable(
cbind(table(small_filtered_spotify_data$popularity_score, res.kmeans.tsne$cluster),
table(small_filtered_spotify_data$popularity_score, res.fuzzy.tsne$cluster))),
type = "latex", file = "MV_Ass_ext_tSNEval_tab.tex")

corrected_rand_table <- t(as.matrix(round(c(
cluster.stats(dd_PCA, as.numeric((as.factor(small_filtered_spotify_data$popularity_score))), 
              res.kmeans.PCA$cluster)$corrected.rand,
cluster.stats(dd_PCA, as.numeric((as.factor(small_filtered_spotify_data$popularity_score))), 
              res.fuzzy.PCA$cluster)$corrected.rand,
cluster.stats(dd_tsne, as.numeric((as.factor(small_filtered_spotify_data$popularity_score))), 
              res.kmeans.tsne$cluster)$corrected.rand,
cluster.stats(dd_tsne, as.numeric((as.factor(small_filtered_spotify_data$popularity_score))), 
              res.fuzzy.tsne$cluster)$corrected.rand),3)))

rownames(corrected_rand_table) <- c("Corrected Rand Index")
colnames(corrected_rand_table) <- c("KmeanPCA", "FuzzyPCA", "KmeantSNE", "FuzzytSNE")
## Save table in current directory to a .tex file
print(xtable(corrected_rand_table), type = "latex", file = "MV_Ass_corrected_rand_tab.tex")

dunn_table <- c(stats_kmeans_PCA$dunn,
                stats_fuzzy_PCA$dunn,
                stats_kmeans_tsne$dunn,
                stats_fuzzy_tsne$dunn)

dunn2_table <- c(stats_kmeans_PCA$dunn2,
                 stats_fuzzy_PCA$dunn2,
                 stats_kmeans_tsne$dunn2,
                 stats_fuzzy_tsne$dunn2)

Final_dunn_table <- rbind(dunn_table, dunn2_table)
rownames(Final_dunn_table) <- c("DunnIdx", "Dunn2Idx")
colnames(Final_dunn_table) <- c("KmeanPCA", "FuzzyPCA", "KmeantSNE", "FuzzytSNE")
## Save table in current directory to a .tex file
print(xtable(round(Final_dunn_table, 3)), type = "latex", file = "MV_Ass_dunn_tab.tex")
