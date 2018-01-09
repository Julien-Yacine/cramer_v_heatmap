# Library -----------------------------------------------------------------

library(data.table) # data mgmt
library(gtools) # combination
library(ggplot2) # graphics
library(plotly) # interactive graphics


# Loading the mushroom dataset  -------------------------------------------
# https://archive.ics.uci.edu/ml/datasets/Mushroom

theUrl <- "https://archive.ics.uci.edu/ml/machine-learning-databases/mushroom/agaricus-lepiota.data"
mushrooms <- read.table(file = theUrl, header = FALSE, sep = ",")

# variables names
# https://rstudio-pubs-static.s3.amazonaws.com/169791_2924e5aa237549ae8a9af2ddabfebaa5.html
mush_names<-c("Class","cap-shape","cap-surface","cap-color","bruises","odor",
              "gill-attachment","gill-spacing","gill-size","gill-color","stalk-shape",
              "stalk-root","stalk-surface-above-ring","stalk-surface-below-ring",
              "stalk-color-above-ring","stalk-color-below-ring","veil-type",
              "veil-color","ring-number","ring-type","spore-print-color",
              "population","habitat")
colnames(mushrooms) <- mush_names
mushrooms <- as.data.table(mushrooms)

# Remove veil-type because the variable only have 1 level
str(mushrooms)
mushrooms[, "veil-type"  := NULL]
cat_var <- colnames(mushrooms)



# Cramer's V calculation --------------------------------------------------

# Function to compute Cramer's V
# https://www.r-bloggers.com/example-8-39-calculating-cramers-v/
cv.test = function(x,y) {
  CV = sqrt(chisq.test(x, y, correct=FALSE)$statistic /
              (length(x)[1] * (min(length(unique(x))[1],length(unique(y))[1]) - 1)))
  return(as.numeric(CV))
}

# Apply the function to the combination of categorical variable
v_cramer_all <- function(cat_var, df){
  cat_var_grid <- data.table(combinations(n = length(cat_var), r = 2, v = cat_var, repeats.allowed = FALSE))
  
  do.call(rbind,
          apply(cat_var_grid, 1, function(x){
            tmp <- as.character(x)
            vec1 <- unlist(df[,tmp[1], with = FALSE])
            vec2 <- unlist(df[,tmp[2], with = FALSE])
            
            data.table(
              variable_x = tmp[1],
              variable_y = tmp[2],
              chi2 = chisq.test(x = vec1, vec2, correct=FALSE)$p.value,
              v_cramer = cv.test(x = vec1, y = vec2)
            )
          }))
  
}

results <- v_cramer_all(cat_var = cat_var, df = mushrooms)



# Heatmap vizualisation with ggplot2  -------------------------------------

g <- ggplot(results, aes(variable_x, variable_y)) +
  geom_tile(aes(fill = v_cramer), colour = "black") +
  theme(axis.text.x=element_text(angle=45, hjust=1)) +
  scale_fill_gradient(low = "white", high = "steelblue") +
  theme_bw() + xlab(NULL) + ylab(NULL) +
  theme(axis.text.x=element_text(angle = -90, hjust = 0)) +
  ggtitle("Cramer's V heatmap")

ggplotly(g)