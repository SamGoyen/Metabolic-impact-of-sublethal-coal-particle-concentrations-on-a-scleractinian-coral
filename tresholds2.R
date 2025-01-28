library(tidyverse)
require(cmdstanr)
options(brms.backend = "cmdstanr", mc.cores = 16)
library(bayesnec)
source("functions.R")
library(ggpubr)

dat <- read.csv("DRC_IC_Raw_Data.csv")
head(dat)
resp_vars <- c("Fv.Fm", "P..gross.", "R", "GL", "GD")

# all_fits <- lapply(resp_vars, FUN = function(i){
#   xvar <- log(dat$Concentration + 0.1)
#   yvar <- dat[, i]
#   # plot(sqrt(xvar), yvar)
#   # str(dat)
# 
#   fit <- try(bnec(yvar~crf(xvar, model = "all"),
#                   data = na.omit(data.frame(xvar,yvar)),
#                   control = list(adapt_delta = 0.995),
#                   iter = 40000, warmup = 39500,
#                   cores = 4))
# 
# })
# names(all_fits) <- resp_vars
# clean_fits <- lapply(all_fits, clean_models)
# save(all_fits, clean_fits, file="bnecfits2.RData")
load("bnecfits2.RData")

# autoplot(all_fits[[1]],xform = function(x){x^2})
# grobs <- lapply(all_fits, autoplot, xform = function(x){x^2})
# ggarrange(plotlist=grobs, labels=resp_vars)
# 

par(mfrow=c(3,2))
lapply(clean_fits, plot, lxform = exp)
lapply(clean_fits, summary)
lapply(clean_fits, rhat)



library("tidyr")
library("stringr")
modtab <- purrr::map_dfr(clean_fits, function(x) {
  x$mod_stats |>
    dplyr::select(model, wi) |>
    dplyr::mutate(wi = round(wi, 3))
}, .id = "herbicide") |>
  tidyr::pivot_wider(names_from = herbicide, values_from = wi) |> 
  data.frame()
colnames(modtab) <- stringr::str_to_title(colnames(modtab))

write.csv(modtab, "model_weights.csv")

xform_fun <- function(x){exp(x)}
library("ggpubr")
round_digits <- function(x) sprintf("%.2f", x)
all_plots <- lapply(clean_fits, function(x) {
  autoplot(x, xform = xform_fun, nec = FALSE) +
    scale_x_continuous(trans = "log", labels = round_digits) +
    theme(axis.title.x = element_blank(),
          axis.title.y = element_blank(),
          strip.background = element_blank(),
          strip.text.x = element_blank()) +
    ggtitle("")
})

names(all_plots)
blank_plot <- ggplot() +theme_void()
  # autoplot(clean_fits[[1]], xform = exp, nec = FALSE, col=NA, fill=NA) +
  # scale_x_continuous(trans = "log", labels = round_digits) +
  # theme(axis.title.x = element_blank(),
  #       axis.title.y = element_blank(),
  #       strip.background = element_blank(),
  #       strip.text.x = element_blank()) +
  # ggtitle("") +
  # theme_void()

figure <- ggpubr::ggarrange(all_plots[["Fv.Fm"]], 
                            blank_plot,
                            all_plots[["R"]], 
                            all_plots[["P"]], 
                            all_plots[["GD"]], 
                            all_plots[["GL"]], 
                            nrow = 3, ncol = 2, 
                            labels = c("a) Fv/Fm", "", 
                                       "b) Respiration", 
                                       "c) Photosynthesis",
                                       "d) Calcification (dark)",
                                       "e) Calcification (light)"),
                            #align = "hv",
                            font.label = list(color = "black", size = 12, face = "plain"))
ggsave(figure, file = "bayesnec_plot.png", height = 10, width = 7)
nsec_vals <- lapply(clean_fits, toxval::nsec, xform=exp)
ec10_vals <- lapply(clean_fits, ecx, xform=xform_fun)
ec50_vals <- lapply(clean_fits, ecx, ecx_val = 50, xform=xform_fun)

names(nsec_vals) <- resp_vars
names(ec10_vals) <- resp_vars
names(ec50_vals) <- resp_vars

rbind(
nsec_vals |> 
  bind_rows(.id = "resp") |> mutate(estimate="n(s)ec"), 
ec10_vals |> 
  bind_rows(.id = "resp") |> mutate(estimate="ec10"),
ec50_vals |> 
  bind_rows(.id = "resp") |> mutate(estimate="ec50")  
) |> 
  mutate(val=round_digits(Q50),
         lw=round_digits(Q2.5),
         up=round_digits(Q97.5)) |> 
  dplyr::select(resp, estimate, val, lw, up) |> 
  arrange(resp, estimate) |> 
  write.csv(file="bayesnec_estimates.csv")

  


  






