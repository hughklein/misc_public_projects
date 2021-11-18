library(tidyverse)
library (StatsBombR)

## Import SB Data ## -------------

Comp <- FreeCompetitions()
Matches <- FreeMatches(Comp)
Matches = Matches %>% filter(competition.competition_name=="UEFA Euro")
data360 <- StatsBombFree360Events(MatchesDF = Matches, Parallel = T)


events <- StatsBombFreeEvents(MatchesDF = Matches, Parallel = T)
events <- allclean(events)
events <- get.opposingteam(events)

data360 = data360 %>% rename(id = event_uuid)

events = events %>% left_join(data360, by = c("id" = "id"))

events = events %>% rename(match_id = match_id.x) %>% select(-match_id.y)


ffs = events %>%
  group_by(team.name) %>%
  filter(type.name=="Pass") %>%
  select(id, match_id, team.name, OpposingTeam, player.name, type.name, minute, second, pass.outcome.name, pass.shot_assist, location.x, location.y, pass.end_location.x, pass.end_location.y, pass.type.name, pass.cross, freeze_frame) %>%
  ungroup()

ffs = ffs %>% unnest(freeze_frame) %>%
  mutate(ff_location.x = (map(location, 1)), ff_location.y = (map(location, 2))) %>%
  select(-location) %>%
  mutate(ff_location.x = as.numeric(ifelse(ff_location.x == "NULL", NA, ff_location.x)), ff_location.y = as.numeric(ifelse(ff_location.y == "NULL", NA, ff_location.y)))


## Clean Cross Data ## -------------

crosses = ffs %>%
#  filter(pass.end_location.x>102 & pass.end_location.y>18 & pass.end_location.y<62) %>%
  filter(is.na(pass.type.name) | pass.type.name=="Recovery" | pass.type.name=="Interception")%>%
  filter(pass.cross==TRUE) %>%
  filter(keeper==FALSE) %>%
  mutate(pass.start_location.x=location.x,
         pass.start_location.y=location.y)

area_fx <- function(i){
  input <- crosses %>% filter(id==i) %>%
    mutate(loc.x = ff_location.x,
           loc.y = ff_location.y,
           ff_location.x = ifelse(ff_location.x<102, 102, ff_location.x),
           ff_location.y = ifelse(ff_location.y>62, 62, ff_location.y),
           ff_location.y = ifelse(ff_location.y<18, 18, ff_location.y))
  
  area <- deldir::deldir(input$ff_location.x, input$ff_location.y, rw=c(102,120,18,62))
  
  area <- area$summary %>%
    select(ff_location.x = x, ff_location.y = y, dir.area) %>%
    left_join(input, by=c('ff_location.x', 'ff_location.y')) %>%
    mutate(team_area = ifelse(teammate=="TRUE" & actor=="FALSE", dir.area, 0),
           opp_area = ifelse(teammate=="FALSE" & actor=="FALSE", dir.area, 0)) %>%
    group_by(id) %>%
    summarize(team_area=sum(team_area),
              opp_area=sum(opp_area))
  
  actor <- input %>%
    filter(actor==TRUE) 
  
  actor_loc.x <- actor$loc.x
  actor_loc.y <- actor$loc.y
  
  dist <- input %>% 
    filter(teammate==FALSE) %>%
    select(loc.x, loc.y, teammate, actor) %>%
    mutate(actor.x = actor_loc.x, 
           actor.y = actor_loc.y,
           distance = (((actor.x-loc.x)^2) + ((actor.y-loc.y)^2))^0.5,
           in_path = case_when(actor.y>40 & actor.y>=loc.y & actor.x<=loc.x ~ 1,
                               actor.y<40 & actor.y<=loc.y & actor.x<=loc.x ~ 1,
                               TRUE ~ 0))
  
  dist_1 <- dist %>%
    arrange(distance) %>%
    slice(1) %>%
    pull(distance)
  
  dist_2 <- dist %>%
    arrange(distance) %>%
    slice(2) %>%
    pull(distance)
  
  dist_1_inpath <- dist %>%
    filter(in_path==1) %>%
    arrange(distance) %>%
    slice(1) %>%
    pull(distance)
  
  dist_1_inpath <- ifelse(is_empty(dist_1_inpath), 0, dist_1_inpath)
  
  area %>% 
    mutate(dist_1 = dist_1,
           dist_2 = dist_2,
           dist_1_inpath = dist_1_inpath)
  
}
  
area <- map(unique(crosses$id), area_fx)
area <- bind_rows(area)


crosses <- crosses %>%
  group_by(team.name, OpposingTeam, id) %>%
  summarise(player.name, pass.start_location.x, pass.start_location.y,
    attackers = sum(teammate==TRUE & ff_location.x>102 & ff_location.y>18 & ff_location.y<62, na.rm = TRUE),
            defenders = sum(teammate==FALSE & ff_location.x>102 & ff_location.y>18 & ff_location.y<62, na.rm = TRUE),
            att_n_def = attackers+defenders,
            att_v_def = attackers-defenders,
            shot_assists = ifelse(is.na(pass.shot_assist), 0, 1)) %>%
  ungroup() %>%
  distinct() %>%
  left_join(area, by="id") %>%
  mutate(area_delta = team_area-opp_area,
         goal_dist = (((pass.start_location.x - 120)^2) + ((pass.start_location.y - 40)^2))^0.5,
         goal_angle = atan2(abs(pass.start_location.x-120), abs(pass.start_location.y-40)),
         dist_1_inpath = ifelse(dist_1_inpath==0, goal_dist, dist_1_inpath))

team <- crosses %>%
  group_by(team.name) %>%
  summarize(avg_att = mean(attackers),
            avg_def = mean(defenders),
            avg_total = mean(att_n_def),
            avg_mismatch = mean(att_v_def),
            avg_area_delta = mean(area_delta),
            avg_team_area = mean(team_area),
            avg_opp_area = mean(opp_area),
            total = n(),
            shot_assists_per = mean(shot_assists)) %>%
  left_join(events %>% select(team.name, match_id) %>%
              group_by(team.name) %>%
              summarize(matches = n_distinct(match_id)), 
            by='team.name') %>%
  mutate(cross_per_match = total/matches) 


## Team Scatter Plot  ## -------------

arw_annotate <- arrow(length = unit(5, 'pt'), type = 'closed')

p <- team %>%
  ggplot(aes(x=avg_area_delta, y=shot_assists_per, label=team.name)) +
  geom_point(aes(size=cross_per_match), alpha=0.75) +
  scale_size(range=c(1,10),name="Crosses per Match") +
  geom_hline(yintercept=mean(team$shot_assists_per), linetype="dashed", alpha=0.25) +
  geom_vline(xintercept=mean(team$avg_area_delta), linetype="dashed", alpha=0.25) +
  annotate(geom="text",x=min(team$avg_area_delta)-15, y=min(team$shot_assists_per)-0.03, label="Limited Control\nLow Success",fontface='italic') +
  annotate(geom="text",x=min(team$avg_area_delta)-15, y=max(team$shot_assists_per)+0.03, label="Limited Control\nHigh Success",fontface='italic') +
  annotate(geom="text",x=max(team$avg_area_delta)+15, y=min(team$shot_assists_per)-0.03, label="Good Control\nLow Success",fontface='italic') +
  annotate(geom="text",x=max(team$avg_area_delta)+15, y=max(team$shot_assists_per)+0.03, label="Good Control\nHigh Success",fontface='italic') +
  ggrepel::geom_label_repel(show.legend=F, family="Roboto") +
  ggpubr::theme_pubr() +
  coord_cartesian(ylim = c(0, 0.35), xlim = c(-400, -150)) +
  theme(text=element_text(family="Roboto")) +
  theme(legend.position = "right",
        plot.caption = element_text(hjust = 0)) +
  labs(
    title = 'Euro 2020: How does the amount of area controlled by attackers in the box impact cross success?',
    #    subtitle = glue::glue('With <b>true</b> goal and xG difference (not a ratio)'),
    x = "Average Area Controlled by Attackers in the Box minus Area Controlled by Defenders in the Box\n(Yards Squared)",
    y = "Shot Assists per Cross",
    caption = 'Note:\n- Data from StatsBomb.'
  ) +
  geom_curve(
    inherit.aes = FALSE,
    aes(x = -180, xend = -200, y = 0.23, yend = 0.187),
    linetype = 1,
    curvature = 0.2,
    color = 'navy',
    arrow=arw_annotate
  ) +
  ggtext::geom_richtext(
    inherit.aes = FALSE,
    data = tibble(
      lab = glue::glue("Hungary probably<br/>should've crossed<br/>more...")
    )
    ,
    aes(
      x = -180,
      y = 0.23,
      label=lab
    ),
    fill = NA_character_,
    label.color = NA_character_,
    hjust = 0,
    color = 'navy'
#    size = pts(10)
  ) 
  
  

base_size <- 8.5
asp <- 1.5
ggsave(
  filename = 'crossing_area.png', 
  plot = p,
  height = base_size, 
  width = base_size * asp
)


## Expected Cross Model ## -------------

library(tidymodels)

crosses_mod <- crosses %>%
  mutate(shot_assists = as.factor(shot_assists)) %>%
  select(-c(player.name, team.name, OpposingTeam, pass.start_location.x, pass.start_location.y))

set.seed(123)
data_split <- initial_split(crosses_mod, prop = 4/5, strata=shot_assists)

train_data <- training(data_split)
test_data  <- testing(data_split)


set.seed(123)
val_set <- validation_split(train_data, 
                            strata = shot_assists, 
                            prop = 0.80)

cross_rec <-
  recipe(shot_assists ~ ., data=train_data) %>%
  update_role(id, new_role="ID") 
  

## GLM ##

lr_mod <- 
  logistic_reg() %>% 
  set_engine("glm")

cross_wflow <- 
  workflow() %>% 
  add_model(lr_mod) %>% 
  add_recipe(cross_rec)

cross_fit <- 
  cross_wflow %>% 
  fit(data = train_data)

coefs <- cross_fit %>% 
  pull_workflow_fit() %>% 
  tidy()


cross_pred <- bind_rows(
  predict(cross_fit, test_data, type = "prob") %>% 
  bind_cols(test_data %>% select(shot_assists, id))
#  predict(cross_fit, train_data, type = "prob") %>% 
#  bind_cols(train_data %>% select(shot_assists, id))
)

cross_pred %>%
  roc_auc(truth=shot_assists, .pred_1)


## GLMNET ##

net_mod <- 
  logistic_reg(penalty = tune(), mixture = 0) %>% 
  set_engine("glmnet")

net_workflow <-
  workflow() %>% 
  add_model(net_mod) %>% 
  add_recipe(cross_rec)

net_reg_grid <- tibble(penalty = 10^seq(-4, -1, length.out = 30))

unregister_dopar <- function() {
  env <- foreach:::.foreachGlobals
  rm(list=ls(name=env), pos=env)
}
unregister_dopar()

net_res <- 
  net_workflow %>% 
  tune_grid(val_set,
            grid = net_reg_grid,
            control = control_grid(save_pred = TRUE),
            metrics = metric_set(roc_auc))

net_plot <- 
  net_res %>% 
  collect_metrics() %>% 
  ggplot(aes(x = penalty, y = mean)) + 
  geom_point() + 
  geom_line() + 
  ylab("Area under the ROC Curve") +
  scale_x_log10(labels = scales::label_number())

net_plot 

top_models <-
  net_res %>% 
  show_best("roc_auc", n = 15) %>% 
  arrange(penalty) 

top_models

net_best <- 
  net_res %>% 
  collect_metrics() %>% 
  arrange(desc(mean)) %>% 
  slice(2)

net_mod <- 
  logistic_reg(penalty = net_best %>% pull(penalty), mixture = 1) %>% 
  set_engine("glmnet")

net_wflow <-
  workflow() %>%
  add_model(net_mod) %>%
  add_recipe(cross_rec)


net_fit <- 
  net_wflow %>% 
  fit(data = train_data)

coefs <- net_fit %>% 
  pull_workflow_fit() %>% 
  tidy()


net_pred <- bind_rows(
  predict(net_fit, test_data, type = "prob") %>% 
    bind_cols(test_data %>% select(shot_assists, id, attackers, defenders, att_v_def, opp_area, team_area, dist_1_inpath, goal_dist))
  #  predict(cross_fit, train_data, type = "prob") %>% 
  #  bind_cols(train_data %>% select(shot_assists, id))
)

net_pred %>%
  roc_auc(truth=shot_assists, .pred_1)



## Individual Expected Cross Plots ## -------------

plot_input <- ffs %>%
  filter(id=="add0a8bf-f248-4501-9e97-731ee473e603") %>%
  mutate(player_type_fill = case_when(
    teammate==TRUE & actor==FALSE ~ "Attacker",
    keeper==TRUE ~ "Goalkeeper",
    actor==TRUE ~ "Crosser",
    TRUE ~ "Defender"
  ))

voronoi <- deldir::deldir(plot_input$ff_location.x, plot_input$ff_location.y, rw=c(85,120,0,80))
  
cross1 <- plot_input %>%
  ggplot() +
  annotate("rect",xmin = 0, xmax = 120, ymin = 0, ymax = 80, fill = NA, colour = "black", size = 0.6) +
  annotate("rect",xmin = 0, xmax = 60, ymin = 0, ymax = 80, fill = NA, colour = "black", size = 0.6) +
  annotate("rect",xmin = 18, xmax = 0, ymin = 18, ymax = 62, fill = NA, colour = "black", size = 0.6) +
  annotate("rect",xmin = 102, xmax = 120, ymin = 18, ymax = 62, fill = NA, colour = "black", size = 0.6) +
  annotate("rect",xmin = 0, xmax = 6, ymin = 30, ymax = 50, fill = NA, colour = "black", size = 0.6) +
  annotate("rect",xmin = 120, xmax = 114, ymin = 30, ymax = 50, fill = NA, colour = "black", size = 0.6) +
  annotate("rect",xmin = 120, xmax = 120.5, ymin =36, ymax = 44, fill = NA, colour = "black", size = 0.6) +
  annotate("rect",xmin = 0, xmax = -0.5, ymin =36, ymax = 44, fill = NA, colour = "black", size = 0.6) +
  annotate("segment", x = 60, xend = 60, y = -0.5, yend = 80.5, colour = "black", size = 0.6)+
  annotate("segment", x = 0, xend = 0, y = 0, yend = 80, colour = "black", size = 0.6)+
  annotate("segment", x = 120, xend = 120, y = 0, yend = 80, colour = "black", size = 0.6)+
  theme(rect = element_blank(),
        line = element_blank()) +
  annotate("point", x = 108 , y = 40, colour = "black", size = 1.05) +
  annotate("path", colour = "black", size = 0.6,
           x=60+10*cos(seq(0,2*pi,length.out=2000)),
           y=40+10*sin(seq(0,2*pi,length.out=2000)))+
  annotate("point", x = 60 , y = 40, colour = "black", size = 1.05) +
  annotate("path", x=12+10*cos(seq(-0.3*pi,0.3*pi,length.out=30)), size = 0.6,
           y=40+10*sin(seq(-0.3*pi,0.3*pi,length.out=30)), col="black") +
  annotate("path", x=107.84-10*cos(seq(-0.3*pi,0.3*pi,length.out=30)), size = 0.6,
           y=40-10*sin(seq(-0.3*pi,0.3*pi,length.out=30)), col="black") +
  geom_point(aes(x = ff_location.x, y = ff_location.y, fill=player_type_fill),
             size = 6, alpha = 0.8, shape=21) +
  labs(title = "Vladimír Coufal Cross, 55:59", subtitle = "Czech Republic vs Croatia, UEFA EURO 2020", caption = "Note:\n- Data from StatsBomb (See: https://github.com/statsbomb/open-data)\n- 'Probability of Cross Assisting a Shot' calculated strictly from pre-cross player locations (i.e. the model does not incorporate the actual cross direction or trajectory).") +
  annotate(geom="label",x=90, y=40, label="Probability of Cross Assisting a Shot = 24.8%",fontface='bold', size = 6, family="Roboto",
           label.padding = unit(0.4, "lines")) +
  geom_segment(
    aes(x=x1, y=y1, xend=x2, yend=y2),
    size=0.75, alpha=0.3,
    data=voronoi$dirsgs,
    linetype=2,
    color= "black") +
  coord_flip(xlim = c(85, 125)) +
  theme(axis.text.x=element_blank(),
      axis.title.x = element_blank(),
      axis.title.y = element_blank(),
      plot.caption=element_text(size=13,family="Roboto", hjust=0, vjust=0.5),
      plot.subtitle = element_text(size = 18, family="Roboto", hjust = 0.5),
      axis.text.y=element_blank(),
      legend.position = "top",
      legend.title=element_blank(),
      legend.text=element_text(size=14,family="Roboto"),
      legend.margin = margin(c(20, 10, -65, 50)),
      legend.key.size = unit(1.5, "cm"),
      plot.title = element_text(margin = margin(r = 10, b = 10), face="bold",size = 24, family="Roboto", colour = "black", hjust = 0.5),
      legend.direction = "horizontal",
      axis.ticks=element_blank(),
      aspect.ratio = c(65/100),
      plot.background = element_rect(fill = "white"),
      strip.text.x = element_text(size=13,family="Roboto"))




plot_input <- ffs %>%
  filter(id=="a8ad0fb5-e7b2-416d-8fcb-5e46d9a41d05") %>%
  mutate(player_type_fill = case_when(
    teammate==TRUE & actor==FALSE ~ "Attacker",
    keeper==TRUE ~ "Goalkeeper",
    actor==TRUE ~ "Crosser",
    TRUE ~ "Defender"
  ))

voronoi <- deldir::deldir(plot_input$ff_location.x, plot_input$ff_location.y, rw=c(85,120,0,80))

cross2 <- plot_input %>%
  ggplot() +
  annotate("rect",xmin = 0, xmax = 120, ymin = 0, ymax = 80, fill = NA, colour = "black", size = 0.6) +
  annotate("rect",xmin = 0, xmax = 60, ymin = 0, ymax = 80, fill = NA, colour = "black", size = 0.6) +
  annotate("rect",xmin = 18, xmax = 0, ymin = 18, ymax = 62, fill = NA, colour = "black", size = 0.6) +
  annotate("rect",xmin = 102, xmax = 120, ymin = 18, ymax = 62, fill = NA, colour = "black", size = 0.6) +
  annotate("rect",xmin = 0, xmax = 6, ymin = 30, ymax = 50, fill = NA, colour = "black", size = 0.6) +
  annotate("rect",xmin = 120, xmax = 114, ymin = 30, ymax = 50, fill = NA, colour = "black", size = 0.6) +
  annotate("rect",xmin = 120, xmax = 120.5, ymin =36, ymax = 44, fill = NA, colour = "black", size = 0.6) +
  annotate("rect",xmin = 0, xmax = -0.5, ymin =36, ymax = 44, fill = NA, colour = "black", size = 0.6) +
  annotate("segment", x = 60, xend = 60, y = -0.5, yend = 80.5, colour = "black", size = 0.6)+
  annotate("segment", x = 0, xend = 0, y = 0, yend = 80, colour = "black", size = 0.6)+
  annotate("segment", x = 120, xend = 120, y = 0, yend = 80, colour = "black", size = 0.6)+
  theme(rect = element_blank(),
        line = element_blank()) +
  annotate("point", x = 108 , y = 40, colour = "black", size = 1.05) +
  annotate("path", colour = "black", size = 0.6,
           x=60+10*cos(seq(0,2*pi,length.out=2000)),
           y=40+10*sin(seq(0,2*pi,length.out=2000)))+
  annotate("point", x = 60 , y = 40, colour = "black", size = 1.05) +
  annotate("path", x=12+10*cos(seq(-0.3*pi,0.3*pi,length.out=30)), size = 0.6,
           y=40+10*sin(seq(-0.3*pi,0.3*pi,length.out=30)), col="black") +
  annotate("path", x=107.84-10*cos(seq(-0.3*pi,0.3*pi,length.out=30)), size = 0.6,
           y=40-10*sin(seq(-0.3*pi,0.3*pi,length.out=30)), col="black") +
  geom_point(aes(x = ff_location.x, y = ff_location.y, fill=player_type_fill),
             size = 6, alpha = 0.8, shape=21) +
  labs(title = "Jens Stryger Larsen Cross, 86:5", subtitle = "Denmark vs Finland, UEFA EURO 2020", caption = "Note:\n- Data from StatsBomb (See: https://github.com/statsbomb/open-data)\n- 'Probability of Cross Assisting a Shot' calculated strictly from pre-cross player locations (i.e. the model does not incorporate the actual cross direction or trajectory).") +
  annotate(geom="label",x=90, y=40, label="Probability of Cross Assisting a Shot = 7.0%",fontface='bold', size = 6, family="Roboto",
           label.padding = unit(0.4, "lines")) + 
  geom_segment(
    aes(x=x1, y=y1, xend=x2, yend=y2),
    size=0.75, alpha=0.3,
    data=voronoi$dirsgs,
    linetype=2,
    color= "black") +
  coord_flip(xlim = c(85, 125)) +
  theme(axis.text.x=element_blank(),
        axis.title.x = element_blank(),
        axis.title.y = element_blank(),
        plot.caption=element_text(size=13,family="Roboto", hjust=0, vjust=0.5),
        plot.subtitle = element_text(size = 18, family="Roboto", hjust = 0.5),
        axis.text.y=element_blank(),
        legend.position = "top",
        legend.title=element_blank(),
        legend.text=element_text(size=14,family="Roboto"),
        legend.margin = margin(c(20, 10, -65, 50)),
        legend.key.size = unit(1.5, "cm"),
        plot.title = element_text(margin = margin(r = 10, b = 10), face="bold",size = 24, family="Roboto", colour = "black", hjust = 0.5),
        legend.direction = "horizontal",
        axis.ticks=element_blank(),
        aspect.ratio = c(65/100),
        plot.background = element_rect(fill = "white"),
        strip.text.x = element_text(size=13,family="Roboto"))


cross2

base_size <- 8.5
asp <- 1.5
ggsave(
  filename = 'cross1.png', 
  plot = cross1,
  height = base_size, 
  width = base_size * asp
)
ggsave(
  filename = 'cross2.png', 
  plot = cross2,
  height = base_size, 
  width = base_size * asp
)


## Cross Characteristic Plots ## -------------


pred <- bind_rows(
  predict(net_fit, test_data, type = "prob") %>% 
    bind_cols(test_data %>% select(shot_assists, id, attackers, defenders, att_v_def, opp_area, team_area, dist_1_inpath, goal_dist)),
  predict(cross_fit, train_data, type = "prob") %>% 
    bind_cols(train_data %>% select(shot_assists, id, attackers, defenders, att_v_def, opp_area, team_area, dist_1_inpath, goal_dist))
)

