library(pacman); p_load(tidyverse, openxlsx2, magrittr, patchwork)
getwd()
setwd("phys417")
# - pacman: is for loading packages without needing to rewrite the word "library()" a bunch
#   - it also doubles as the "install.packages()" function
# - tidyverse: imports a bunch of differnt libraries such as ggplot2, dplyr, and stringr
# - openxlsx2: is used to read excel files
# - magrittr: i like the assignment pipe operator (%<>%), but it's not best practice
# - patchwork: used for figure 10 to "add ggplots" together into a single figure

# ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~
# ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ FIGURE 7~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~
# ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~

# reading in the excel sheets, from specific sheets
rsw <- read_xlsx("PHYS417_Photon-Lab.xlsx", sheet = "RSO_layers", cols = c(1,2,4), rows = 1:28) %>%
    mutate("Instrument" = "RSO50")
rew <- read_xlsx("PHYS417_Photon-Lab.xlsx", sheet = "RadEye_layers", cols = c(1,2,4), rows = 1:28) %>%
    mutate("Instrument" = "RadEyeG")
#bind_rows(re, rs) %>% View

# putting the dataframes on top of eachother
b1 <- bind_rows(rew, rsw) %>%
    transmute(`Distance from Source (cm)`, `Predicted mR/hr` = `Exposure Rate (mR/hr)`,
       `Experimental mR/hr` = `Expt mR/hr`, `Instrument`)

# lm() is a function that fits a line (slope and intercept) to a set of data, and is done with the "twiddle" (~) operator 
f1 <- lm(`Experimental mR/hr` ~ `Predicted mR/hr`, b1)
eqstr <- paste0("y = ", format(f1$coefficients[1], digits = 2),
                " + ", format(f1$coefficients[2], digits = 2),
                "*x") 

rsofit  <- lm(`Expt mR/hr` ~ `Exposure Rate (mR/hr)`, data = rsw)
reyefit <- lm(`Expt mR/hr` ~ `Exposure Rate (mR/hr)`, data = rew)

# outputs a summary of the fitted curve
summary(rsofit)
summary(reyefit)

bind_rows(rew, rsw) %>%
    transmute(
        #"Instrument" = factor("Instrument", levels = c("RSO50","RadEyeG")),
       `Distance from Source (cm)`,
       `Predicted mR/hr` = `Exposure Rate (mR/hr)`,
       `Experimental mR/hr` = `Expt mR/hr`,
       `Instrument`
        ) %>% 
    ggplot(aes(x = `Predicted mR/hr`, y = `Experimental mR/hr`, color = Instrument)) +
    geom_point() +
    scale_color_manual(values = c( # setting the color values
        "RSO50" = "#1c8bc3",
        "RadEyeG" = "#f8687c"
    )) +
    theme_minimal() +
    theme( # specific theme parameters such as axis title font sizes
        axis.text = element_text(size = 12),
        axis.title = element_text(size = 12),
        axis.line= element_line(linewidth = 0.1),
        axis.ticks = element_line(linewidth = 0.25),
        legend.text = element_blank(),
        legend.background = element_blank(),
        legend.title = element_blank(),
        legend.position = "none",
        strip.text = element_text(size = 16)
    ) +
    #geom_smooth( linewidth = 0.3, se = F) +
    #annotate(geom = "text", label = "") +
    #scale_x_continuous(breaks = seq(20,300,by=40)) +
    geom_abline(slope = 1, color = "#9b9797", linetype = 2) + 
    facet_wrap(~Instrument, scales = "free") # making multiple plots into a single figure





# ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~
# ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ FIGURE 8~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~
# ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~
re <- read_xlsx(
    "PHYS417_Photon-Lab.xlsx",
     sheet = "RadEye_layers",
      cols = 1:3)
rs <- read_xlsx("PHYS417_Photon-Lab.xlsx", 
    sheet = "RSO_layers", 
    cols = 1:3)
#bind_rows(re, rs) %>% View
bind_rows(re, rs) %>% mutate( # mutating adds a column with the specified inputs, in this case i have added "RadEye" and "RSO50" ...
    "Instrument" = rep(c("RadEyeG","RSO50"), each = 162) # ... to a column so that i can facet the plots accordingly
    ) %>%
    mutate(
        `Shielding Status` = factor(`Shielding Status`,  # factors are like "levels" that tell the script what order something should be in
            levels = c(
            "Predicted Unshielded", 
            "Experimental Unshielded",
            "Predicted QVL",
            "Experimental QVL", 
            "Predicted TVL",
            "Experimental TVL"))
        ) %>% 
        arrange(`Shielding Status`) %>% # arranging by the specified factor
    ggplot(aes(x = `Distance from Source (cm)`, y = `Exposure Rate (mR/hr)`, color = `Shielding Status`,
    shape = `Shielding Status`)) +
    geom_point() + 
    scale_color_manual(
        values = c(
            "Predicted Unshielded" = "#1f3174", 
            "Experimental Unshielded" = "#8da7fe",
            "Predicted QVL" = "#bc2121",
            "Experimental QVL" = "#f8687c", 
            "Predicted TVL" = "#1c8bc3",
            "Experimental TVL" = "#17C5C5"
            )) +
    scale_shape_manual(
        values = c(
            "Predicted Unshielded" = 2, 
            "Experimental Unshielded" = 19,
            "Predicted QVL" = 2,
            "Experimental QVL" = 19, 
            "Predicted TVL" = 2,
            "Experimental TVL" = 19
            )) +
    theme_minimal() +
    theme(
        axis.text = element_text(size = 12),
        axis.title = element_text(size = 12),
        axis.line= element_line(linewidth = 0.1),
        axis.ticks = element_line(linewidth = 0.25),
        legend.text = element_text(size = 12, margin = margin(l = 3),
                                lineheight = 0.9),
        legend.key.height = unit(15, "pt"),
        legend.key.width = unit(10, "pt"),
        legend.position = "inside",
        legend.position.inside = c(0.85, 0.85),
        legend.background = element_blank(),
        legend.key.justification = "top",
        legend.title = element_blank(),
        strip.text = element_text(size = 16)
    ) +
    #scale_y_log10()+
    scale_x_continuous(breaks = seq(20,300,by=40)) +
    facet_wrap(~Instrument, scales = "free")







# ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~
# ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ FIGURE 9~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~
# ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~

Reye.error <- read_xlsx(
    "PHYS417_Photon-Lab.xlsx",
     sheet = "RadEye_error",
      cols = 1:3)
RSO.error <- read_xlsx("PHYS417_Photon-Lab.xlsx", 
    sheet = "RSO_error", 
    cols = 1:3)
#bind_rows(re, rs) %>% View
bind_rows(Reye.error, RSO.error) %>% mutate(
    "Instrument" = rep(c("RadEyeG","RSO50"), each = 75)
    ) %>%
    mutate(
        `Shielding Status` = factor(`Shielding Status`, 
            levels = c(
            "Unshielded", 
            "QVL", 
            "TVL"))
        ) %>% 
        arrange(`Shielding Status`) %>%
    ggplot(aes(x = `Distance from Source (cm)`, y = `Percent Error`, color = `Shielding Status`,
    shape = `Shielding Status`)) +
    geom_point() + 
    scale_color_manual(
        values = c(
            "Unshielded" = "#8da7fe",
            "QVL" = "#f8687c", 
            "TVL" = "#17C5C5"
            )) +
    scale_shape_manual(
        values = c(
            "Unshielded" = 1, 
            "QVL" = 2,
            "TVL" = 3
            )) +
    theme_minimal() +
    theme(
        axis.text = element_text(size = 12),
        axis.title = element_text(size = 12),
        axis.line= element_line(linewidth = 0.1),
        axis.ticks = element_line(linewidth = 0.25),
        legend.text = element_text(size = 12, margin = margin(l = 3),
                                lineheight = 0.9),
        legend.key.height = unit(15, "pt"),
        legend.key.width = unit(10, "pt"),
        legend.position = "inside",
        legend.position.inside = c(0.95, 0.85),
        legend.background = element_blank(),
        legend.key.justification = "top",
        legend.title = element_blank(),
        strip.text = element_text(size = 16)
    ) +
    #scale_y_log10()+
    scale_x_continuous(breaks = seq(20,300,by=40)) +
    scale_y_continuous(labels = scales::percent_format(accuracy = 1)) + # percent format
    facet_wrap(~Instrument, scales = "free")








# ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~
# ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ FIGURE 10 ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~
# ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~
hp <- read_xlsx("PHYS417_Photon-Lab.xlsx", sheet = "HP210", cols = c(1,4,5)) %>% na.omit
# transmute is like mutate but only includes the imputs rather than necessarily adding a column only
hp %<>% transmute(`Distance from Source (cm)`, `Incident photon rate (γ/sec)` = `Predicted DPS`, `Experimental CPS`)

dpslim <- 0.5e6
dpslim2 <- 0.15e6

# regression for each portion of the dataset
f1 <- lm(`Experimental CPS` ~ 0 +  `Incident photon rate (γ/sec)` , data = filter(hp,  `Incident photon rate (γ/sec)`  < dpslim), weights = 1 / `Experimental CPS`)
eqstr <- paste0("y = ", format(f1$coefficients[1], digits = 2), "*x"); eqstr
f2 <- lm(`Experimental CPS` ~ 0 +  `Incident photon rate (γ/sec)` , data = hp, weights = 1 / `Experimental CPS`)
eqstr2 <- paste0("y = ", format(f2$coefficients[1], digits = 2), "*x"); eqstr2
f3 <- lm(`Experimental CPS` ~ 0 +  `Incident photon rate (γ/sec)` , data = filter(hp,  `Incident photon rate (γ/sec)`  < dpslim2), weights = 1 / `Experimental CPS`)
eqstr3 <- paste0("y = ", format(f3$coefficients[1], digits = 2), "*x"); eqstr3

p_dist <- ggplot(hp, aes(x = `Distance from Source (cm)`)) +
  #geom_point(aes(y = `Incident photon rate (γ/sec)`,    color = "Predicted"), size = 1.75) +
  geom_point(aes(y = `Experimental CPS`, color = "Experimental"), size = 1.75) +
  labs(y = "CPS", color = NULL) +
  theme(
    axis.text = element_text(size = 12),
    axis.title = element_text(size = 12),
    axis.line= element_line(linewidth = 0.1),
    axis.ticks = element_line(linewidth = 0.25),
    axis.title.y = element_text(size = 15),
    axis.title.x = element_text(size = 15)
    )

p_eff <- ggplot(hp, aes(x = `Incident photon rate (γ/sec)`, y = `Experimental CPS`)) +
    geom_point(color = "#38d09d") +
    xlab("Incident photon rate (γ/sec)") +
    geom_abline(linetype = "dashed", color = "black", slope = f1$coefficients[1]) +
    annotate("text", x = 0.7e6, y = 8000, label = str_c("When φ < ", dpslim, ":\n" ,eqstr)) +
    annotate("segment", x = dpslim, xend = dpslim, y = 0, yend = 7000) +
    annotate("segment", x = 0e0, xend = dpslim, y = 7000, yend = 7000) +
    annotate("text", x = 1.7e6, y = 12000, label = str_c("For all φ vals: \n" ,eqstr2)) +
    annotate("segment", x = dpslim2, xend = dpslim2, y = 0, yend = 2000) +
    annotate("segment", x = 0e0, xend = dpslim2, y = 2000, yend = 2000) +
    annotate("text", x = 0.15e6, y = 2500, label = str_c("φ < 0.15e+06: \n", eqstr3), size = 2.6) +
    theme(
    axis.title.x = element_text(size = 15),
    axis.title.y = element_text(size = 15)
    )

# patchwork dependency
# we can add plots together insead of just faceting them, this was done because the two plots rely on different datasets ...
# ... one plot relied on the CPS vs. distance, whereas the other relied on the efficiency curve
(p_dist + p_eff) & theme_minimal() + 
    theme(
        axis.text = element_text(size = 12),
        axis.title = element_text(size = 12),
        axis.line= element_line(linewidth = 0.1),
        axis.ticks = element_line(linewidth = 0.25),
        legend.text = element_text(size = 12, margin = margin(l = 3),
        lineheight = 0.9),
        legend.key.height = unit(15, "pt"),
        legend.key.width = unit(10, "pt"),
        legend.position = "none",
        legend.position.inside = c(0.85, 0.85),
        legend.background = element_blank(),
        legend.key.justification = "top",
        legend.title = element_blank(),
        axis.title.y = element_text(size = 15),
        axis.title.x = element_text(size = 15)
    )