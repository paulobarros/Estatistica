# Load necessary libraries
library(ggplot2)
library(tidyverse)
library(ggtext)







ggplot(data = data.frame(x = c(-3, 3)), aes(x)) +
  stat_function(fun = dnorm, 
                xlim = c(-3,-2),
                geom = "area",
                fill = "lightblue") +
  stat_function(fun = dnorm, 
                xlim = c(2,3),
                geom = "area",
                fill = "lightblue") +
  stat_function(fun = dnorm, 
                xlim = c(-2,2),
                geom = "area",
                fill = "gray95") +
  stat_function(fun = dnorm, n = 101, args = list(mean = 0, sd = 1),
                linewidth=1) +
  geom_curve(x = -2.3, y = 0.01,
             xend = -2.5, yend = 0.1,
             color = "gray20",
             arrow = arrow(
               angle = 30, 
               length = unit(0.15, "inches"),
               ends = "last", type = "closed"
             ),
             linewidth = .8,
             curvature = 0.3) +
  geom_curve(x = 2.3, y = 0.01,
             xend = 2.5, yend = 0.1,
             color = "gray20",
             arrow = arrow(
               angle = 30, 
               length = unit(0.15, "inches"),
               ends = "last", type = "closed"
             ),
             linewidth = .8,
             curvature = -0.3) +
  geom_richtext( x = -2.5, y = 0.13, label = "&alpha;/2",
                 family = "Barlow Condensed Medium",
                 fill = NA,
                 size = 7,
                 label.color = NA) +
  geom_richtext( x = 2.5, y = 0.13, label = "&alpha;/2",
                 family = "Barlow Condensed Medium",
                 fill = NA,
                 size = 7,
                 label.color = NA) +
  geom_richtext( x = 0, y = 0.2, label = "95%",
                 family = "Barlow Condensed Medium",
                 fill = NA,
                 size = 7,
                 label.color = NA) +
  geom_vline(xintercept = -2,
             linetype = 2,
             color = "gray60",
             linewidth = .8) +
  geom_vline(xintercept = 2,
             linetype = 2,
             color = "gray60",
             linewidth = .8) +
  scale_x_continuous(breaks = c(-3,-2,-1,0,1,2,3), limits = c(-3,3)) +
  scale_y_continuous( breaks=NULL ) +
  labs(x = "μ", y = element_blank()) +
  theme_bw(base_size = 18, base_family = "Barlow Condensed Medium") +
  theme(
    panel.grid = element_blank(),
    axis.title.x = element_markdown(face = "italic"),
    panel.border =  element_rect(linewidth = 1)
  )


library(ggplot2)
library(tidyverse)
library(ggtext)



  
  set.seed(134)
  
  fdist <- tibble(x = rf(100,3,20))
  
  den <- density(fdist$x)
  
  fdist <- tibble(x = den$x, y = den$y)
  
ggplot(data = fdist, aes(x,y)) +
    geom_area(data = fdist |>
                filter(x >= 3),
              aes(x = x, y = y),
              fill = "gray60")  +
    geom_area(data = fdist |>
                filter(x <= 3),
              aes(x = x, y = y),
              fill = "gray90")  +
    geom_segment(
      aes(x = 3.0, xend = 3.0, y = 0, yend = 0.07),
      linetype = 2,
      linewidth = .6) +
    geom_line(linewidth=1) +
    annotate("text",x= 1, y = 0.027, label = "0.99",
             family = "Barlow Condensed Medium",
             size = 6)+
    annotate("text",x= 3.25, y = 0.027, label = "0.01",
             family = "Barlow Condensed Medium",
             size = 6)+
    annotate("text",x= 1, y = 0.2, label = "RA",
             family = "Barlow Condensed Medium",
             size = 7)+
    annotate("text",x= 3.1, y = 0.027, label = "RR",
             family = "Barlow Condensed Medium",
             size = 7)+
    geom_curve(data = tibble(x = 3, y = 0.045,
                             xend = 2.8, yend = 0.11),
               aes(x=x,y=y,xend=xend,yend=yend),
               color = "gray20",
               arrow = arrow(
                 angle = 30, 
                 length = unit(0.1, "inches"),
                 ends = "last", type = "closed"
               ),
               linewidth = .5,
               curvature = -0.5) +
    geom_curve(data = tibble(x = 3.6, y = 0.03,
                             xend = 3.8, yend = 0.10),
               aes(x=x,y=y,xend=xend,yend=yend),
               color = "gray20",
               arrow = arrow(
                 angle = 30, 
                 length = unit(0.1, "inches"),
                 ends = "last", type = "closed"
               ),
               linewidth = .5,
               curvature = 0.4) +
    geom_richtext(data = tibble(x = 2.7, y = 0.13, label = "<i>F</i><sub>0.01</sub>(3,20) = 4.94"),
                  aes(x,y,label = label),
                  family = "Barlow Condensed Medium",
                  size = 6,
                  label.color = NA,
                  fill = NA
    ) +
    geom_richtext(data = tibble(x = 3.8, y = 0.13, label = "<i>F</i><sub>CALC</sub> = 5.40"),
                  aes(x,y,label = label),
                  family = "Barlow Condensed Medium",
                  size = 6,
                  label.color = NA,
                  fill = NA
    ) +
    scale_x_continuous(limits = c(0,4), expand = c(0,0)) +
    scale_y_continuous(expand = expansion(mult = c(0,0))) +
    theme_classic(base_size = 18, base_family = "Barlow Condensed Medium") +
    coord_cartesian(clip = "off") +
    theme(
      panel.grid = element_blank(),
      axis.title.x = element_markdown(face = "italic"),
      #panel.border =  element_rect(linewidth = 1),
      axis.text = element_blank(),
      axis.ticks = element_blank(),
      axis.title = element_blank(),
      plot.margin = margin(t = 20,  # Top margin
                           r = 10,  # Right margin
                           b = 60,  # Bottom margin
                           l = 10),
      axis.line = element_line(arrow = arrow(
        angle = 40, 
        length = unit(0.1, "inches"),
        ends = "last", type = "closed"
      ))
      
    )
  





