library(RColorBrewer)
library(cowplot)
library(reshape2)
library(scales)
library(tidyverse)

# open the adjancency matrix describing the scientific collaboration evolution rates of french cities bet. 2000 and 2013
# data: SCI Expanded (articles, reviews, letters). NETSCIENCE project (UMR LISST, 2017)
# MESRI data : Number of students (primary enrolment in public higher education without double counts)
# revisited in 2020, UMR Géographie-cités, Paris

netm <- read_rds("data/frcollab_evolmatrix_2000-2013.rds")

library(igraph)

g <- graph_from_adjacency_matrix(netm, mode = c("undirected"), weighted = T,
                            diag = T, add.colnames = T, add.rownames = NA)
E(g)$weight


# classic heatmap with base R

# sources :
# https://kateto.net/network-visualization
# https://stackoverflow.com/questions/57395558/how-to-show-legend-in-heatmap

palf <- colorRampPalette(c("gold", "dark orange"))

heatmap(netm[c(1:6, 9),c(1:6, 9)], # do not display the categories 7 and 8 which stand for "Villes sans étudiants du public" and "Autres villes françaises"
        Rowv = NA, Colv = "Rowv",
        # cex.lab = 1.5, cex.axis = 1.5, cex.main = 1.5, cex.sub = 1.5,
        scale = "none", margins = c(12, 12), xlab = "", ylab = "", main = "Taux de croissance des coopérations \nentre catégories de villes entre 2000 et 2013",
        col = colorRampPalette(brewer.pal(7, "Oranges"))(25)) # col = palf(100)

legend(x = 0.6, y = 0.2, legend = c("Croissance faible", "Croissance moyenne", "Croissance forte"), # alternative legend pos: "bottomright"
       fill = colorRampPalette(brewer.pal(7, "Oranges"))(3))

# classy heatmap with ggplot2

# sources:
# http://www.sthda.com/english/wiki/ggplot2-quick-correlation-matrix-heatmap-r-software-and-data-visualization
# https://gist.github.com/murrayds/bb00f0880bef0046e8235fb74b44c059 (related to https://twitter.com/dakotasmurray/status/1308761325485666317?s=20)


# define functions to keep only the upper or lower triangle of a matrix

# Get lower triangle of the correlation matrix
get_lower_tri <- function(x){
  x[upper.tri(x)] <- NA
  return(x)}

# Get upper triangle of the correlation matrix
get_upper_tri <- function(x){
  x[lower.tri(x)]<- NA
  return(x)
}

# select the desired dataset
# getting rid of the categories 7 and 8

  netmtri <- get_lower_tri(netm[c(1:6, 9),c(1:6, 9)])
# netmtri <- get_upper_tri(netm[c(1:6, 9),c(1:6, 9)])

############################################################################################

# rearrange the data for ggplot either with the reshape2 package (melt) or with dplyr

## with melt
 melted_netmtri <- melt(netmtri, na.rm = F)

## with dplyr

# option 2: uploading the data with an alternative format is possible
# netm <- read_tsv("data/frcollab_evolmatrix_2000-2013.tsv", col_names = F)
# colnames = rownames
# colnames(netm) <- (c("Catégories de villes", netm$X1))

# option2: using tidyr: convert the ajacency matrix into a tibble

# melted_netmtri <- netmtri %>%
                 # as.table() %>%
                 # as_tibble(.name_repair = "unique", rownames = NA) %>%
                 # rename( Var = names(.))  %>%
                 # rename( value = Var3)

# ggplot2 Heatmap

plot.new()

ggplot(data = melted_netmtri, aes(Var1, reorder(Var2, desc(Var2)), fill = value*100)) +
  geom_tile(color = "white") +
  scale_fill_gradient(low = "yellow", high = "red", space = "Lab",
                      name = "Taux\nde croissance", na.value = "lightgrey") +
   theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, vjust = 1,
                                   size = 12, hjust = 1)) +
  coord_fixed()


# svg output with captions

svg(paste("Heatmap_2000_2013_fr_collab_final2.svg"), width = 7, height = 6)


melted_netmtri <- melted_netmtri %>%
                  filter(Var1 != "Paris (plus de 400 000 étudiants)")


m <- melted_netmtri %>%
  ggplot(aes(x = Var1, y = reorder(Var2, desc(Var2)), fill = value*100)) +
  # Use the geom_tile function to draw the squares
  geom_tile(color = "lightgrey") +
  scale_fill_gradient(low = "yellow", high = "red", space = "Lab",
                      name = "Croissance \n des collaborations \n scientifiques (%)", na.value = "white") +
  theme_minimal()+
  theme(plot.title = element_text(face = "bold"), # , hjust = -0.3
        plot.subtitle = element_text(face = "italic"),
        axis.title = element_text(color = "#4c4c4c"),
        axis.text.x = element_text(angle = 45, vjust = 1,
                                   size = 10, hjust = 1),
        axis.text.y = element_text(size = 10),
        panel.grid = element_blank())+
  xlab("Catégories de villes") + ylab("") +
  ggtitle("Dynamiques des collaborations scientifiques \nfrançaises entre 2000 et 2013", subtitle = "Catégorisation des villes françaises selon leurs effectifs étudiants")

plot.new()

ggdraw(add_sub(m, fontface = "italic", size = 7.5, color = "black", x = -1.1, y = 0.5, hjust = 0, vjust = 0.5, fontfamily = "sans", lineheight = 0.5,
               label =
                  " Clef de lecture : Entre 2000 et 2013, les collaborations entre les villes de 2000 à 10 000 étudiants et l'international ont augmenté de plus de 60 %.\n
                 Sur la même période, les collaborations scientifiques de Paris avec l'international n'ont augmenté que de 40 %,\n
                 et celles des 11 villes entre 30 000 et 400 000 étudiants et l'international, de 45 %. \n
                 Source: Science Citation Index Expanded (articles, recensions et lettres) ; \n
                 Nbre. d'inscriptions principales d'étudiants du sup. public en 2014, sans doubles comptes (données ouvertes du MESR).\n
                 Comptage entier fractionné à l'agglomération urbaine, moyenne mobile sur 3 ans. \n
                 Données du projet Labex SMS NETSCIENCE (UMR LISST, 2017). Réalisation : MM, UMR Géographie-cités, 2020."

))

dev.off()

# Informations supplémentaires :
# Volume de collaborations par classe en 2013 :\n
# Paris : 15 000 ; Villes entre 30 000 et 400 000 ets. : 23 000 ; Villes entre 10 000 et 30 000 ets. : 10 000 ;\n
# Villes entre 2000 et 10 000 ets. : 2200 ; Villes entre 700 et 2000 ets. : 665 ; Villes entre 20 et 700 ets. : 650.\n

# take care!
