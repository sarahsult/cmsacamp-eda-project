library(tidyverse)
# Load all regular season passes from the 2020 regular season: ---------------------------
library(nflfastR)
nfl_passing_plays<-as_tibble(nfl_passing_plays)
head(nfl_passing_plays)


#1D: KEEPING! QB hit+sack ---------------------------------------------------------------------
#Hypothesis: figuring out what teams have worse offensive lines
nfl_passing_plays %>%
  group_by(posteam) %>%
  summarize(total_hit = sum(qb_hit)+sum(sack)) %>%
  ungroup() %>%
  mutate(posteam = fct_reorder(posteam, total_hit))%>%
  ggplot(aes(x=posteam, y=total_hit)) +
  geom_col()+
  theme_bw()+
  theme(axis.text.x = element_text(size = 8, angle = 90))+
  labs(x= "Possessing Team",
       y= "QB hits + sacks",
       title = "Comparing NFL Team's Offensive Line Strength",
       caption = "Data courtesy of nflfastR")
#conclusion: ranking



#2D: KEEPING!! if a QB is hit are they more likely to throw an interception -------------------
#Hypothesis: if QB is hit --> more likely to throw interception

#need these row numbers to do conditional probability
nfl_complete <- nfl_passing_plays %>%
  filter(complete_pass == 1)

nfl_not_complete <- nfl_passing_plays %>%
  filter(complete_pass == 0)

nfl_passing_plays %>%  
  filter(sack != 1) %>%                 #remove sacks because it artificially inflates 
                                      #the value of hit/no interception
  group_by(qb_hit, interception, complete_pass) %>%
  summarize(count = n(),
            joint_prob = case_when(
              complete_pass == 0 ~ count/nrow(nfl_not_complete),
              TRUE ~ count/nrow(nfl_complete)
              )) %>%
  ungroup() %>%
  mutate(qb_hit_name = case_when(
    qb_hit == 0 ~ "No hit",
    TRUE ~ "Hit"
  ), interception_name = case_when(
    interception == 0 ~ "No interception",
    TRUE ~ "Interception"
  ), complete_pass = case_when(
    complete_pass == 0 ~ "Incomplete pass",
    TRUE ~ "Complete pass"
  )) %>%
  ggplot(aes(x=qb_hit_name, y=interception_name)) +
  geom_tile(aes(fill=count), color="white")+
  geom_text(aes(label = round(joint_prob, digits=4)), color = "white")+
  facet_wrap(~ complete_pass, ncol=2) +
  scale_fill_viridis_b()+
  theme_bw()+
  theme(legend.position = "bottom", legend.key.width = unit(2,"cm")) +
  labs(
    x = "QB Hit",
    y = "Interception",
    title = "Interception Not More Likely When Hit",
    caption = "Data courtesy of nflfastR"
  )
#conclusion: incomplete passes are more common, but of those interception is not more likely


#KEEPING: density on yards gained (could also just keep it at 2 graphs separated -------------
          #by complete pass)
#Hypothesis: what is a common number of yards for a passing play?

library(patchwork)
yards_gained_dens_total <- nfl_passing_plays %>%
  ggplot(aes(x=yards_gained)) +
  geom_density() +
  geom_rug(alpha = .3) +
  theme_bw() +
  theme(legend.title = element_blank()) +
  labs(x="Yards Gained",
       y = "Num of Plays")

yards_gained_ecdf_total <- nfl_passing_plays %>%
  ggplot(aes(x=yards_gained)) +
  stat_ecdf()+
  geom_rug(alpha = .3) +
  theme_bw()+
  theme(legend.title = element_blank()) +
  labs(x="Yards Gained",
       y = "Prop of Plays")

yards_gained_dens <- nfl_passing_plays %>%
  mutate(complete_pass = case_when(
    complete_pass == 0 ~ "Incomplete Pass",
    TRUE ~ "Complete Pass"
  )) %>%
  ggplot(aes(x=yards_gained, color = complete_pass)) +
  geom_density() +
  geom_rug(alpha = .3) +
  theme_bw() +
  theme(legend.title = element_blank()) +
  scale_color_manual(values = c("darkblue", "darkorange")) +
  labs(x="Yards Gained",
       y = "Num of Plays")

yards_gained_ecdf <- nfl_passing_plays %>%
  mutate(complete_pass = case_when(
    complete_pass == 0 ~ "Incomplete Pass",
    TRUE ~ "Complete Pass"
  )) %>%
  ggplot(aes(x=yards_gained, color=complete_pass)) +
  stat_ecdf()+
  geom_rug(alpha = .3) +
  theme_bw()+
  theme(legend.title = element_blank()) +
  scale_color_manual(values = c("darkblue", "darkorange")) +
  labs(x="Yards Gained",
       y = "Prop of Plays")

((yards_gained_dens_total + theme(axis.title.x = element_blank())+ yards_gained_dens + theme(axis.title.y = element_blank(), axis.title.x = element_blank(), legend.position = "none")) / 
    ( yards_gained_ecdf_total + yards_gained_ecdf + theme(axis.title.y = element_blank()))) + 
  plot_layout(guides="collect") + plot_annotation(title="Passing Plays Don't Get Many Yards", caption = "Data courtesy of nflfastR")

#conclusion: the first 50% of observed passing plays only gets you a few yards (because there 
  # are so many incomplete plays. Of the complete plays, 50% get you 10/15 yards)


#KEEPING Clustering: total hits and total epa -----------------------------------------------------
#try to collapse to one entry a thrower (could stand to get rid of people who didn't throw much)
nfl_passing_plays_total_hits_and_epa <- nfl_passing_plays %>%
  group_by(passer_player_name) %>%
  summarise(total_hits = sum(qb_hit) + sum(sack),
            total_epa = sum(epa),
            total_throws = n())%>%
  ungroup()

nfl_passing_plays_total_hits_and_epa %>%
  ggplot(aes(x=total_throws)) +
  stat_ecdf()+
  geom_vline(xintercept = 5, linetype = "dashed", color = "darkred")+
  theme_bw()+
  labs(x = "Total Throws",
       y = "Proportion")
#~37.5% of data threw less than 5 times
  
nfl_passing_plays_total_hits_and_epa_over5 <- nfl_passing_plays_total_hits_and_epa %>%
  filter(total_throws > 5)

play_dist_hits_and_epa <- dist(dplyr::select(nfl_passing_plays_total_hits_and_epa_over5, total_hits, total_epa))

hits_epa_complete_hclust <- hclust(play_dist_hits_and_epa, method="complete")

nfl_passing_plays_total_hits_and_epa_over5 %>%
  mutate(player_clusters = as.factor(cutree(hits_epa_complete_hclust, k=3)),) %>%
  ggplot(aes(x=total_hits, y=total_epa, color = player_clusters))+
  geom_point(alpha = .75)+
  theme_bw()+
  theme(legend.position = "bottom")+
  ggthemes::scale_color_colorblind()+
  labs(x = "Total Hits",
       y = "Total Expected Points Added",
       title = "Categorizing Players by Total Hits and Total EPA",
       caption = "Data courtesy of nflfastR")





# 2D: NOT USING Downs and yards --------------------------------------------------------------------------
nfl_passing_plays %>%
  ggplot(aes(x=down, y=yards_gained)) +
  geom_point() +
  theme_bw()



#1D: NOT USING distribution of total yards ---------------------------------------------------------------
#Hypothesis: ??
nfl_passing_plays %>%
  ggplot(aes(x=yards_gained))+
  stat_ecdf()+
  theme_bw()




#2D: NOT USING when each qb scores most of their touchdowns in the game ----------------------------------- 
#Hypothesis: ??
nfl_passing_plays %>%
  filter(passer_player_name %in% c("T.Brady", "D.Watson", "P.Mahomes", "L.Jackson", "D.Lock"),
         touchdown=="1") %>%
  group_by(qtr, passer_player_name) %>%
  summarize(total_TD = sum(touchdown)) %>%
  ggplot(aes(x = passer_player_name, y = total_TD, fill = factor(qtr))) +
  geom_col()+
  scale_fill_brewer(palette = "Paired")+
  theme_bw()+
  labs(
    title = "Touchdowns by QB in 2020",
    x = element_blank(),
    y = "Total Touchdowns",
    fill = "Quarter"
  )




#2D: NOT USING no huddle passes vs. non no huddle passes ------------------------------------------------
#Hypothesis: no huddle plays are more successful b/c you can exploit defensive line
nfl_passing_plays %>%
  ggplot(aes(x=no_huddle, fill = complete_pass)) +  #kind of expected this to have 4 bars
  geom_bar(position = "dodge") +
  theme_bw()
  






#NOT USING huddle no huddle and yards gained -----------------------------------------------------------
#Hypothesis: no huddle plays are more successful b/c you can exploit defensive line
nfl_passing_plays %>%
  mutate(no_huddle = case_when(
    no_huddle == 0 ~ "Huddle",
    TRUE ~ "No huddle"
  )) %>%
  ggplot(aes(x=play_id, y= yards_gained)) +
  geom_point(alpha = .2) +
  theme_bw() +
  facet_wrap(~ no_huddle)

#conclusion: how can we tell if there is a difference or if there is just more data points?




#2D: NOT USING yards gained and pass location ----------------------------------------------------------
#Hypothesis: one area of the field might be better to pass to than another??
nfl_passing_plays %>%
  filter(!is.na(pass_location)) %>%
  ggplot(aes(x=yards_gained, color = pass_location)) +
  geom_freqpoly()+
  theme_bw()+
  theme(legend.position = "bottom")


# NOT USING win probability added and completed passes ---------------------------------------------------
# nfl_passing_plays %>%
#   filter(posteam == "KC") %>%
#   group_by(posteam, game_id) %>%
#   mutate(total_comp_passes = sum(complete_pass)) %>%
#   ggplot(aes(x=total_comp_passes, y=wpa, color = posteam)) +
#   geom_point()+
#   theme_bw()







#2D: NOT USING Yards gained based on pass length ------------------------------------------------------
#Hypothesis: short over long passes are more successful
nfl_passing_plays %>%
  filter(!is.na(pass_length), complete_pass == 1) %>%
  ggplot(aes (x = play_id, y = yards_after_catch, color = yards_gained)) +
  geom_point(alpha = .2) +
  theme_bw() +
  scale_colour_viridis_b()+
  facet_wrap(~ pass_length)

#conclusion: Short passes lead to more yards after catch / more complete passes?? But less 
  #total yards than successful deep passes (duh)

#NOT USING Clustering: air yards and expected points added ---------------------------------------------

nfl_complete_passing_plays <- nfl_passing_plays %>%
  filter(complete_pass == 1)

play_dist <- dist(dplyr::select(nfl_passing_plays, air_yards, epa))
#play_dist <- dist(dplyr::select(nfl_complete_passing_plays, air_yards, epa))

airyards_epa_complete_hclust <- hclust(play_dist, method="complete")

nfl_passing_plays %>%
#nfl_complete_passing_plays %>%
  mutate(play_clusters = as.factor(cutree(airyards_epa_complete_hclust, k=3))) %>%
  ggplot(aes(x=air_yards, y=epa, color = play_clusters))+
  geom_point(alpha = .3)+
  theme_bw()+
  theme(legend.position = "bottom")




#NOT USING Clustering: yards gained and epa -----------------------------------------------------------
play_kmeans <- kcca(dplyr::select(nfl_passing_plays, yards_gained, epa),
                    k = 2, control = list(initcent = "kmeanspp"))

nfl_passing_plays %>%
  mutate(play_clusters = as.factor(play_kmeans@cluster)) %>%
  ggplot(aes(x = yards_gained, y = epa, color = play_clusters)) +
  geom_point() +
  ggthemes::scale_color_colorblind() +
  theme_bw() +
  theme(legend.position = "bottom")





#NOT USING Clustering : total hits per thrower and total completed passes -----------------------------
#might not be great because this just shows that more play time you get hit more and then you 
  #have more time to accumulate completed passes

#trying to get the total number of throws by each person so I can create a cutoff for the graph
nfl_passing_plays_with_total <- nfl_passing_plays %>%
  mutate(total_throws = table(nfl_passing_plays$passer_player_name)[passer_player_name])%>%
  
  
nfl_passing_plays %>%
  group_by(passer_player_name) %>%
  summarise(total_hit = sum(qb_hit), total_comp_pass = sum(complete_pass)) %>%
  ungroup() %>%
  ggplot(aes(x=total_hit, y = total_comp_pass)) +
  geom_point() +
  ggthemes::scale_colour_colorblind() +
  theme_bw()+
  theme(legend.position = "bottom")









nfl_density_compare <- nfl_passing_plays %>%
  ggplot(aes(x = yards_gained,
             color = as.factor(complete_pass))) +
  geom_density() +
  geom_rug(alpha = 0.3) +
  theme_bw() +
  labs(x = "Yards Gained",
       y = "Number of Passing Plays") +
  scale_color_manual(values = c("darkblue","darkorange")) +
  theme(
    axis.title.y = element_blank(),
    axis.title.x = element_blank(),
    plot.background = element_rect(fill = "grey95"),
    panel.background = element_rect(fill = "grey95"),
    legend.background = element_rect(fill = "grey95"),
    legend.key = element_rect(fill = "grey95"),
    panel.grid.minor = element_blank(),
    text = element_text(family = "Century", size = 12),
    plot.title = element_text(size = 17),
    axis.text.x = element_text(size = 8),
    legend.position = "none"
  )
nfl_ecdf_compare <- nfl_passing_plays %>%
  mutate(complete_pass = case_when(
    complete_pass == 0 ~ "Incomplete Pass",
    TRUE ~ "Complete Pass"
  )) %>%
  ggplot(aes(x = yards_gained,
             color = as.factor(complete_pass))) +
  stat_ecdf() +
  geom_rug(alpha = 0.3) +
  theme_bw() +
  labs(x = "Yards Gained",
       y = "Proportion of Plays") +
  scale_color_manual(values = c("darkblue","darkorange")) +
  theme(
    axis.title.y = element_blank(),
    plot.background = element_rect(fill = "grey95"),
    panel.background = element_rect(fill = "grey95"),
    legend.background = element_rect(fill = "grey95"),
    legend.key = element_rect(fill = "grey95"),
    panel.grid.minor = element_blank(),
    text = element_text(family = "Century", size = 12),
    plot.title = element_text(size = 17),
    axis.text.x = element_text(size = 8),
    legend.title = element_blank()
  )
nfl_density <- nfl_passing_plays %>%
  ggplot(aes(x = yards_gained)) +
  geom_density() +
  geom_rug(alpha = 0.3) +
  theme_bw() +
  labs(x = "Yards Gained",
       y = "Number of Plays") +
  theme(
    axis.title.x = element_blank(),
    plot.background = element_rect(fill = "grey95"),
    panel.background = element_rect(fill = "grey95"),
    legend.background = element_rect(fill = "grey95"),
    legend.key = element_rect(fill = "grey95"),
    panel.grid.minor = element_blank(),
    text = element_text(family = "Century", size = 12),
    plot.title = element_text(size = 17)
  )
nfl_ecdf <- nfl_passing_plays %>%
  ggplot(aes(x = yards_gained)) +
  stat_ecdf() +
  geom_rug(alpha = 0.3) +
  theme_bw() +
  labs(x = "Yards Gained",
       y = "Proportion of Plays") +
  theme (
    plot.background = element_rect(fill = "grey95"),
    panel.background = element_rect(fill = "grey95"),
    legend.background = element_rect(fill = "grey95"),
    legend.key = element_rect(fill = "grey95"),
    panel.grid.minor = element_blank(),
    text = element_text(family = "Century", size = 12),
    plot.title = element_text(size = 17),
    axis.text.x = element_text(size = 8)
  )
nfl_density + nfl_density_compare + nfl_ecdf + nfl_ecdf_compare + plot_layout(guides = 'collect')





