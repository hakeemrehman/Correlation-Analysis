'Correlogram
------------'
# Visualization of a correlation matrix using ggplot2
library(ggplot2)
library(ggcorrplot)

data(mtcars)

# Correlation matrix
corr <- round(cor(mtcars), 1)
corr

# Compute a correlation matrix p-values
p.mat <- cor_pmat(mtcars)
head(p.mat)

# Plot the correlation matrix
'----------------------------'
# with default method = "square" 
ggcorrplot(corr) 

# With method = "circle"
ggcorrplot(corr, method = "circle")

# using hierarchical clustering
ggcorrplot(corr, method = "circle", hc.order = TRUE, 
           outline.color = "white")

# Add correlation coefficients
ggcorrplot(corr, method = "circle", hc.order = TRUE, 
           outline.color = "white", lab = TRUE)

# Add correlation significance level
ggcorrplot(corr, method = "circle", hc.order = TRUE, 
           outline.color = "white", lab = TRUE, p.mat = p.mat)

# Get the lower triangle of the correlogram
ggcorrplot(corr, hc.order = TRUE, type = "lower", lab = TRUE, 
           lab_size = 3, method="square",
           colors = c("red", "white", "green"), 
           title="Correlogram of mtcars", 
           ggtheme=theme_bw)

