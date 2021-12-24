# Original testing file with generativeart package

devtools::install_github("cutterkom/generativeart")

library(generativeart)

# Setup per documentation at cutterkom/generativeart

# set the paths
IMG_DIR <- "img/"
IMG_SUBDIR <- "everything/"
IMG_SUBDIR2 <- "handpicked/"
IMG_PATH <- paste0(IMG_DIR, IMG_SUBDIR)

LOGFILE_DIR <- "logfile/"
LOGFILE <- "logfile.csv"
LOGFILE_PATH <- paste0(LOGFILE_DIR, LOGFILE)

# create the directory structure
generativeart::setup_directories(IMG_DIR, IMG_SUBDIR, IMG_SUBDIR2, LOGFILE_DIR)



## Example

# include a specific formula, for example:
my_formula <- list(
  x = quote(runif(1, -1, 1) * x_i^2 - sin(y_i^5) + cos(x_i) + y_i),
  y = quote(runif(1, -1, 1) * y_i^3 - cos(x_i^2))
)

# call the main function to create five images with a polar coordinate system
generativeart::generate_img(formula = my_formula, nr_of_img = 3, polar = TRUE, filetype = "png", color = "black", background_color = "white")


####
## generative art experiement

angle <- 19 * pi / 180
points <- 1500

t <- (1:points)*angle
x <- sin(t)
y <- cos(t)

df <- data.frame(t, x, y)

p <- ggplot(df, aes(x*t, y*t))
p + geom_point(size = 80, alpha = 0.1, shape=1) +
  theme(panel.background = element_rect(fill = "white", colour = "darkmagenta"),
        title =  element_blank(), axis.ticks =  element_blank(),
        axis.text =  element_blank(), panel.grid =element_blank(),
        legend.position = "none")


###

install.packages("aRtsy")
library(aRtsy)

canvas_flow(colors=c("green","pink","white"), background = "black", lwd=.5, stepmax=.01, lines=500)
canvas_turmite(colors=c("green","pink","white"), background = "black")


###
seq(-3,3, by = 0.01) %>% 
  expand.grid(x=., y=.) %>% 
  ggplot(aes(x = 1- x-sin(y^2), y = 1 + y - cos(x^2), color=x*y))+
  geom_point(alpha = 0.05, shape = 20, size = 0)+
  theme_void()+
  coord_polar()+
  labs(subtitle = "Flower")

##

N <- 30
cols <- rainbow(N)
cols <- c("#EF892D", "#FEC784", "#45555F", "#93ADC5", "#BF49DB")
#set.seed(43)

tibble(line = 1:N %% 5,
       x1 = runif(N, -1, 1),
       y1 = runif(N, -1, 1), 
       x2 = runif(N, -1, 1),
       y2 = runif(N, -1, 1), 
       size = runif(N, 0, 3),
       alpha = runif(N, 0.1, 0.9)) %>% 
  ggplot()+
  geom_segment(aes(x = x1, y = y1, xend = x2, yend = y2, size = size, alpha = alpha, color = as.factor(line)),show.legend = FALSE)+
  geom_point(aes(x = x1+runif(1,0,1), y = y1+runif(1, 0,1), size = size, alpha = alpha, color = as.factor(line)),show.legend = FALSE)+
  scale_color_manual(values = cols)+
  scale_size_continuous(range = c(2,20))+
  #  coord_polar() +
  theme_void()+
  labs(subtitle = "Rainbow Lines")

