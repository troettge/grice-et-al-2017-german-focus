
#### Script for plots used in Grice, Ritter, Niemann, & Roettger

#### authors: Timo B. Roettger, Simon Ritter
#### date:    8 Feb 2016

#### required packages ####
library(dplyr)
library(ggplot2)
library(purrr)
library(irr)
library(grid)
library(gridExtra)
library(tidyr)


#### load in data ####
## set working directory
setwd("/Users/simon/Dropbox/Focus\ Paper\ 2/JPHON_Paper/RE-Revision/Script\ and\ data\ Re-Revision/Data_Script_For_Publication_With_Transcriber_Diffs")

## set up output folder for plots, will be created if it does not exist
dir.create(file.path(".", "Plots"))

## load in data
focus = read.csv("focus_data_intonation_production.csv")
perc.data = read.csv("focus_data_intonation_perception.csv")

## wrap data frame for more tidy print
focus <- tbl_df(focus)


#### aggregate and prepare for plots ####
## aggregate means
focus_agg <-
  focus %>%
  group_by(Intended, Speaker) %>%
  summarise(mean_AlignmentVowel = round(mean(AlignmentVowel, na.rm = T), 1),
            mean_TargetHeight = round(mean(TargetHeight, na.rm = T), 2),
            mean_Onglide = round(mean(Onglide, na.rm = T), 2),
            mean_Displacement = round(mean(DisplacementOpeningGesture, na.rm = T), 2),
            mean_Duration = round(mean(DurationOpeningGesture, na.rm = T), 2),
            mean_RelativeAlignment = round(mean(RelativeAlignmentSyllable, na.rm = T), 2)
  )

## append means to df
focus = full_join(focus, focus_agg)

## change factor levels for plots
focus$Intended = factor(focus$Intended, levels = c("contrastive","narrow","broad"))


#### set plot parameters ####
## store plot parameters for violin plot
plot_parameters_violin = list(coord_flip() ,
                              stat_summary(fun.y = mean, geom = "point", size = 2, color = "black"),
                              scale_fill_manual(values = c("#CC6666", "#9999CC", "#66CC99"), breaks = c("broad", "narrow", "contrastive"), name = "Focus type"),
                              scale_colour_manual(values = c("#CC6666", "#9999CC", "#66CC99"), breaks = c("broad", "narrow", "contrastive"), name = "Focus type", guide = FALSE),
                              guides(fill = guide_legend(title = "Focus Type", title.position = "top")))

## store plot parameters for scatter plot
plot_parameters_jitter = list(coord_flip(),
                              scale_colour_manual(values = c("#CC6666", "#9999CC", "#66CC99"), breaks = c("broad", "narrow", "contrastive"), name = "Focus type"),
                              facet_grid(Speaker ~ .),
                              guides(fill = guide_legend(title = "focus type", title.position = "top")))
                                  
## store plot parameters for plot                                
theme_parameters =   list(theme_classic(),
                     theme(legend.position = "right",
                     legend.title  =  element_text(size = 15),
                     legend.text = element_text(size = 15),
                     legend.key.size = unit(1.5, 'lines'),
                     strip.text.y = element_text(size = 15, face = "bold"),
                     strip.background = element_blank(),
                     panel.border = element_rect(colour = "black",fill = NA),
                     axis.text = element_text(size = 12),
                     axis.title = element_text(size = 15, face = "bold"),
                     axis.ticks.x = element_line(size = 0.5, linetype = 'solid'), 
                     axis.ticks.y = element_blank(),
                     plot.title = element_text(size = 15, face = "bold")))



#### plot measures #####
## plot Onglide ====
# Plot onglide for all speakers
onglide_violin <-
  ggplot(focus, aes(x = Intended, y = Onglide, fill = Intended)) +
  geom_hline(aes(yintercept = mean_Onglide, colour = Intended), linetype = "longdash") +
  ylab("Onglide (semitones)") +
  scale_y_continuous(breaks = seq(-10, 15, by  =  5), limits  =  c(-10, 15)) +
  geom_violin() +
  plot_parameters_violin + 
  theme_parameters +
  theme(axis.text.y = element_blank(), axis.title.y = element_blank()) +
  facet_grid(Speaker ~ .)

# plot onglide for speaker separately
plot_onglide_for_speaker = function(data, speaker) {
  speaker_subset = data[data$Speaker == speaker,]
  onglide_violin =
    ggplot(speaker_subset, aes(x = Intended, y = Onglide, fill = Intended)) +
      geom_hline(aes(yintercept = mean_Onglide, colour = Intended), linetype = "longdash") +
      ylab("Onglide (semitones)") +
      scale_y_continuous(breaks = seq(-10, 15, by  =  5), limits  =  c(-10, 15)) +
      geom_violin() +
      plot_parameters_violin + 
      theme_parameters +
      theme(axis.text.y = element_blank(), axis.title.y = element_blank())
  return(onglide_violin)
}

## plot Alignment ====
# plot Alignment for all speakers
alignment_violin <-
  ggplot(focus, aes(x = Intended, y = AlignmentVowel,  fill = Intended)) +
  geom_hline(aes(yintercept = mean_AlignmentVowel, colour = Intended), linetype = "longdash") +
  ylab("Alignment (ms)") +
  scale_y_continuous(breaks = seq(-200, 200, by = 100), limits = c(-200, 200 )) +
  geom_violin() +
  plot_parameters_violin +
  theme_parameters +
  theme(axis.text.y = element_blank(), axis.title.y = element_blank()) +
  facet_grid(Speaker ~ .)

# plot Alignment for speaker separately
plot_alignment_for_speaker = function(data, speaker) {
  speaker_subset = data[data$Speaker == speaker,]
  alignment_violin <-
  ggplot(speaker_subset, aes(x = Intended, y = AlignmentVowel,  fill = Intended)) +
    geom_hline(aes(yintercept = mean_AlignmentVowel, colour = Intended), linetype = "longdash") +
    ylab("Alignment (ms)") +
    scale_y_continuous(breaks = seq(-200, 200, by = 100), limits = c(-200, 200 )) +
    geom_violin() +
    plot_parameters_violin +
    theme_parameters +
    theme(axis.text.y = element_blank(), axis.title.y = element_blank())
  return(alignment_violin)
}

## plot Relative Alignment ====
rel_alignment_violin =
  ggplot(focus, aes(x = Intended, y = RelativeAlignmentSyllable*100,  fill = Intended)) +
  geom_hline(aes(yintercept = mean_RelativeAlignment*100, colour = Intended), linetype = "longdash") +
  ylab("Relative Alignment in Proportion \n to the Duration of the Accented Syllable (%)") +
  scale_y_continuous(breaks = seq(-70, 150, by = 20), limits = c(-70, 150 )) +
  geom_violin() +
  plot_parameters_violin +
  theme_parameters +
  theme(axis.text.y = element_blank(), axis.title.y = element_blank()) +
  facet_grid(Speaker ~ .)

## plot Target Height ====
# plot target height for all speakers
target_height_violin <-
  ggplot(focus, aes(x = Intended, y = TargetHeight,  fill = Intended)) +
  geom_hline(aes(yintercept = mean_TargetHeight, colour = Intended), linetype = "longdash") +
  ylab("Target Height (semitones)") +
  scale_y_continuous(breaks = seq(0, 20, by = 5), limits = c(0, 20)) +
  geom_violin() +
  plot_parameters_violin +
  theme_parameters +
  theme(axis.text.y = element_blank(), axis.title.y = element_blank()) +
  facet_grid(Speaker ~ .)

# plot target height for speaker separately
plot_target_height_for_speaker = function(data, speaker) {
  speaker_subset = data[data$Speaker == speaker,]
  target_height_violin <-
    ggplot(speaker_subset, aes(x = Intended, y = TargetHeight,  fill = Intended)) +
    geom_hline(aes(yintercept = mean_TargetHeight, colour = Intended), linetype = "longdash") +
    ylab("Target Height (semitones)") +
    scale_y_continuous(breaks = seq(0, 20, by = 5), limits = c(0, 20)) +
    geom_violin() +
    plot_parameters_violin +
    theme_parameters +
    theme(axis.text.y = element_blank(), axis.title.y = element_blank())
  return(target_height_violin)
}

## plot Displacement of Opening Gesture ====
displacement_violin =
  ggplot(focus, aes(x = Intended, y = DisplacementOpeningGesture, fill = Intended)) +
  geom_hline(aes(yintercept = mean_Displacement, colour = Intended), linetype = "longdash") +
  ylab("Displacement of Opening Gesture (mm)") +
  scale_y_continuous(breaks = seq(0, 30, by  =  5), limits  =  c(-5, 30)) +
  geom_violin() +
  plot_parameters_violin +
  theme_parameters +
  facet_grid(Speaker ~ .)

## plot Duration of Opening Gesture ====
duration_violin =
  ggplot(focus, aes(x = Intended, y = DurationOpeningGesture, fill = Intended)) +
  geom_hline(aes(yintercept = mean_Duration, colour = Intended), linetype = "longdash") +
  ylab("Duration of Opening Gesture (ms)") +
  scale_y_continuous(breaks = seq(45, 175, by  = 25), limits  =  c(45, 175)) +
  geom_violin() +
  plot_parameters_violin +
  theme_parameters +
  facet_grid(Speaker ~ .)

## plot Reference F0 210 ms after accented vowel ====
reference_scatter =
ggplot(focus, aes(x = Intended, y = refL, color = Intended)) +
  geom_jitter(size = 3) + 
  ylab("Reference point (Hz) \n 210 ms after target word") +
  plot_parameters_jitter +
  theme_parameters +
  facet_grid(Speaker ~ .)


## extract legend of plot (helper function) ====
g_legend = function(a.gplot) {
  tmp = ggplot_gtable(ggplot_build(a.gplot))
  leg = which(sapply(tmp$grobs, function(x) x$name) == "guide-box")
  legend = tmp$grobs[[leg]]
  return(legend)
}




#### plot proportions of pitch accents ####
## aggregate pitch accent distribution overall ====
focus_prop_all =
  focus %>%
  group_by(Intended, PitchAccentConsensus) %>%
  summarise(n = n()) %>%
  mutate(proportion = (n / sum(n)) * 100)

## aggregate pitch accent distribution per speaker ====
focus_prop_speaker <-
  focus %>%
  group_by(Speaker, Intended, PitchAccentConsensus) %>%
  summarise(n = n()) %>%
  complete(PitchAccentConsensus, fill=list(n = 0)) %>%
  mutate(proportion = (n / sum(n)) * 100)

## change factor levels for plots ====
focus_prop_all$Intended = factor(focus_prop_all$Intended, levels = c("broad","narrow","contrastive"))
focus_prop_all$PitchAccentConsensus = factor(focus_prop_all$PitchAccentConsensus, levels = c("L+H*","H*","H+!H*"))
focus_prop_speaker$Intended = factor(focus_prop_speaker$Intended, levels = c("broad","narrow","contrastive"))
focus_prop_speaker$PitchAccentConsensus = factor(focus_prop_speaker$PitchAccentConsensus, levels = c("L+H*","H*","H+!H*"))

## change ordering by pitch accents for plot ====
focus_prop_all = focus_prop_all[order(focus_prop_all$PitchAccentConsensus),]
focus_prop_speaker = focus_prop_speaker[order(focus_prop_speaker$PitchAccentConsensus),]

## plot pitch accent distribution overall as stacked bar chart ====
accent_proportion_overall =
  ggplot(focus_prop_all, aes(x = Intended, y = proportion, fill = PitchAccentConsensus)) +
    geom_bar(stat = "identity", colour = "black") +
    theme_bw() + 
    scale_fill_manual(values = c("black", "grey", "white"), guide = guide_legend(reverse=FALSE)) +
    theme_parameters +
    ylim(0,101) +
    ylab("Proportion of Accent Type (%)") +
    xlab("Focus Type") +
    guides(fill=guide_legend(title="Accent Type"))

## plot pitch accent distribution per speaker in one plot as stacked bar chart ====
accent_proportion_speaker =
  ggplot(focus_prop_speaker, aes(x = Intended, y = proportion, fill = PitchAccentConsensus)) +
    geom_bar(stat = "identity", colour = "black") +
    theme_bw() + 
    scale_fill_manual(values = c("black", "grey", "white"), guide = guide_legend(reverse = TRUE)) +
    ylim(0,101) +
    ylab("Proportion of each pitch accent\n within focus type (%)") +
    xlab("\nFocus Type") +
    facet_wrap(~Speaker, ncol = 3, scales = "free") +
    theme_parameters +
    ylab("Proportion of Accent Type (%)") +
    xlab("Focus Type") +
    guides(fill=guide_legend(title="Accent Type"))

## plot pitch accent distribution per speaker in separate plot as stacked bar chart ====
plot_accent_distribution_for_speaker = function(data, speaker) {
  speaker_subset = data[data$Speaker == speaker,]
  accent_proportion_plot <-
    ggplot(speaker_subset, aes(x = Intended, y = proportion, fill = PitchAccentConsensus)) +
    geom_bar(stat = "identity", colour = "black") +
    theme_bw() + 
    scale_fill_manual(values = c("black", "grey", "white"), guide = guide_legend(reverse=FALSE)) +
    ylim(0,101) +
    ylab("Proportion of Accent Type (%)") +
    xlab("Focus Type") +
    theme_parameters +
    guides(fill=guide_legend(title="Accent Type"))
  return(accent_proportion_plot)
}

## plot pitch accent distribution per speaker in separate plot as heat map ====
plot_accent_distribution_for_speaker_heat_map = function(data, speaker) {
  speaker_subset = data[data$Speaker == speaker,]

  speaker_subset$Intended = factor(speaker_subset$Intended, levels = c("contrastive","narrow","broad"))
  speaker_subset$PitchAccentConsensus = factor(speaker_subset$PitchAccentConsensus, levels = c("H+!H*","H*","L+H*"))
  
  heat_map_accent_types = ggplot(data = speaker_subset, aes(x = PitchAccentConsensus, y = Intended, fill = proportion)) +
    geom_tile(colour = "black") +
    #geom_text(aes(label = round(proportion, 1)), color="red") + # Numbers in tiles
    scale_fill_gradient(low = "white", 
                        high = "black", 
                        breaks = seq(0, 100, by = 20),
                        limits = c(0,100),
                        guide = "colourbar") +
    theme_classic() +
    theme(axis.text = element_text(size = 12, color="black"), 
          axis.title = element_text(size = 15, face = "bold"), 
          legend.title = element_text(size = 15, color="black"),
          axis.text.y = element_text(angle = 90, hjust = 0.5)) +
    scale_x_discrete(position = "top") +
    xlab("Pitch Accent Type") +
    guides(fill = guide_colourbar(title = "Proportion"))
  return(heat_map_accent_types)
}

heat_map_f1_production = plot_accent_distribution_for_speaker_heat_map(focus_prop_speaker, "F1")
heat_map_f2_production = plot_accent_distribution_for_speaker_heat_map(focus_prop_speaker, "F2")
heat_map_f3_production = plot_accent_distribution_for_speaker_heat_map(focus_prop_speaker, "F3")
heat_map_m1_production = plot_accent_distribution_for_speaker_heat_map(focus_prop_speaker, "M1")
heat_map_m2_production = plot_accent_distribution_for_speaker_heat_map(focus_prop_speaker, "M2")

#### plot perception heat maps ####

## Factor Rated.As and rename levels
perc.data$Rated.As = factor(perc.data$Rated.As)
levels(perc.data$Rated.As)[levels(perc.data$Rated.As) == "1"] = "background"
levels(perc.data$Rated.As)[levels(perc.data$Rated.As) == "2"] = "broad"
levels(perc.data$Rated.As)[levels(perc.data$Rated.As) == "3"] = "narrow"
levels(perc.data$Rated.As)[levels(perc.data$Rated.As) == "4"] = "contrastive"

## Split for speakers
perc.data.f1 = perc.data[perc.data$Speaker == "F1", ]
perc.data.f2 = perc.data[perc.data$Speaker == "F2", ]
perc.data.f3 = perc.data[perc.data$Speaker == "F3", ]
perc.data.m1 = perc.data[perc.data$Speaker == "M1", ]
perc.data.m2 = perc.data[perc.data$Speaker == "M2", ]

## Actually plot heat map 
plot_heat_map <- function(perc.data.speaker, speaker) {
  prop.table.speaker = prop.table(
    table(perc.data.speaker$Intended.Focus, perc.data.speaker$Rated.As), 1)
  prop.df.speaker = as.data.frame(prop.table.speaker)
  colnames(prop.df.speaker) <- c("Intended", "Rated", "Proportion")
  
  # With or without background
  prop.df.speaker = prop.df.speaker[prop.df.speaker$Intended != "background",]
  prop.df.speaker = prop.df.speaker[prop.df.speaker$Rated != "background",]
  
  prop.df.speaker$Intended = factor(prop.df.speaker$Intended, levels(prop.df.speaker$Intended)[c(3, 4, 2, 1)])
  heat_map = ggplot(data = prop.df.speaker, aes(x = Rated, y = Intended, fill = Proportion*100)) +
    geom_tile(colour = "black") +
    #geom_text(aes(label = round(Proportion*100, 1)), color="red") + # Numbers in tiles
    scale_fill_gradient(low = "white", 
                        high = "black", 
                        breaks = seq(0, 100, by = 20),
                        limits = c(0, 100),
                        guide = "colourbar") +
    theme_classic() +
    theme(axis.text = element_text(size = 12, color="black"), 
          axis.title = element_text(size = 15, face = "bold"), 
          legend.title = element_text(size = 15, color="black"), 
          axis.text.y = element_text(angle=90, hjust=0.5)) +
    scale_x_discrete(position = "top") +
    guides(fill = guide_colourbar(title = "Proportion"))
}

heat_map_f1_perception = plot_heat_map(perc.data.f1, "F1")
heat_map_f2_perception = plot_heat_map(perc.data.f2, "F2")
heat_map_f3_perception = plot_heat_map(perc.data.f3, "F3")
heat_map_m1_perception = plot_heat_map(perc.data.m1, "M1")
heat_map_m2_perception = plot_heat_map(perc.data.m2, "M2")


#### combine heat maps (production + perception) ====
combined_heat_map_plots = arrangeGrob(
  textGrob("F1"),
  heat_map_f1_production,
  heat_map_f1_perception,
  textGrob("F2"),
  heat_map_f2_production,
  heat_map_f2_perception,
  textGrob("F3"),
  heat_map_f3_production,
  heat_map_f3_perception,
  textGrob("M1"),
  heat_map_m1_production,
  heat_map_m1_perception,
  textGrob("M2"),
  heat_map_m2_production,
  heat_map_m2_perception,
  ncol = 3,
  nrow = 5,
  widths = c(0.5,1.5,1.5)
)


#### plot the differences accross transcribers ====
difference_props = focus[focus$DifferenceType != "S",] %>% 
  group_by(DifferenceType) %>% 
  summarise (n = n()) %>% 
  mutate(freq = n / sum(n))
levels(difference_props$DifferenceType) <- c("H+!H* vs. H*","L+H* vs. H*","One label missing", "S")

transcriber_diff =
ggplot(difference_props, aes(x = DifferenceType, y = freq*100)) +
  geom_bar(stat="identity") +
  ylim(0,60) +
  xlab("Difference Type") +
  ylab("Proportion of Differences Across Transcribers (%)") +
  theme_bw()


### save all plots ====
ggsave("plots/onglide_violin.png", 
       plot = onglide_violin, width = 7, height = 9)
ggsave("plots/alignment_violin.png",
       plot = alignment_violin, width = 7, height = 9)
ggsave("plots/rel_alignment_violin.png",
       plot = rel_alignment_violin, width = 7, height = 9)
ggsave("plots/target_height_violin.png",
       plot = target_height_violin, width = 7, height = 9)
ggsave("plots/displacement_violin.png",
       plot = displacement_violin, width = 7, height = 9)
ggsave("plots/duration_violin.png",
       plot = duration_violin, width = 7, height = 9)
ggsave("plots/reference_scatter.png",
       plot = reference_scatter, width = 7, height = 9)
ggsave("plots/accent_proportion_overall.png",
       plot = accent_proportion_overall, width = 7, height = 6)
ggsave("plots/accent_proportion_speaker.png",
       plot = accent_proportion_speaker, width = 10, height = 9)
ggsave("plots/transcriber_diff.png",
       plot = transcriber_diff, width = 7, height = 6)
ggsave("plots/combined_heat_map_plots.png",
       plot = combined_heat_map_plots, width = 10, height = 15)


#### calculate interrater reliability ####
kappa2(focus[,c("PitchAccentTranscriber1","PitchAccentTranscriber2")], "unweighted")
