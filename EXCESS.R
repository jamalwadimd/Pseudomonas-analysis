
# ?EXCESS CODE
#################

# # Separate tables for Ps. aeruginosa and All GNB
# 
# ## Figure A: Years vs. Ps. aeruginosa count

combined_files %>%
  filter(
    Organism.Name == "Ps.aeruginosa",
    Patient.Location %in% c("Ward","ICCU")
  ) %>%
  count(Years, Organism.Name) %>%
  ggplot(
    aes(x = Years, y = n))+
  geom_point()+
  geom_smooth(level = 0.95)+      # 95% C.I.
  labs(
    title = expression("Figure A: The Frequency of " *italic("Ps. aeruginosa")* " Isolates From All Sources Over Fifteen Years (2010 - 2024)"),
    x = "Years",
    y = "Frequency of Ps. aerugionsa")+
  scale_x_continuous(breaks = unique(combined_files$Years))+
  theme_minimal()

## Figure B Years vs. GNB_binomial count

combined_files %>%
  filter(
    GNB_binomial == "Yes",
    Patient.Location %in% c("ICCU", "Ward")
  ) %>%
  count(Years, GNB_binomial) %>%
  ggplot(
    aes(x = Years, y = n))+
  geom_point()+
  geom_smooth(level = 0.95)+      # 95% C.I.
  labs(
    title = "Figure B: The Frequency of All Gram-Negative Bateria over Fifteen Years (2010 - 2024)",
    y = "The Frequency of Gram-Negative Bacteria"
  )+
  scale_x_continuous(breaks = unique(combined_files$Years))+
  theme_minimal()

### Putting the above (commented out) two curves side by side as ONE FIGURE ###



################################################################################


##############

# getting the Pseudomonas ratio and the glm model for trends

# ## Ratio of P. aeruginosa with respect to all GNB, and trends
ratio <- counts_Ps %>%
  left_join(counts_GNB, by = "Years") %>%
  mutate(
    PsGNB_ratio = round((Ps_count/GNB_count)*100, 3))

# Plot for the pseudomonas ratio alone

ggplot(pseudomonas_ratio,aes(x = Years, y = PsGNB_ratio))+
  geom_point()+
  geom_line()+
  geom_smooth(ci = 0.95, method = lm, se = TRUE)

# glm model_P

model_P <-glm(
  PsGNB_ratio ~ Years,
  family = poisson,
  data = pseudomonas_ratio)
summary (model_P)

################################################################################



#####################################################################################################
#### for Reference and teaching rename_with
# Pseudomonas_aeruginosa %>%
# rename_with(~ str_replace_all(., "Expertized", "AST"), ends_with("Expertized"))

# Reassigning factor values for MIC to numeric:
#
#              (<=0.03 = 0.029)           (<=1 = 0.09)           (<=20 = 19.9)
#              (<=0.06 = 0.059)           (<=2 = 1.9)            (>=16 = 16.1)
#              (<=0.12 = 0.119)           (>=2 = 2.1)            (>=32 = 32.1)
#              (<=0.25 = 0.2495)          (<=4  = 3.9)           (>=64 = 64.1)
#              (<=0.5 = 0.49)             (>=8 = 8.1)            (>=128 = 128.1)
#              (>=0.5 = 0.51)             (<=10 = 9.9)           (>=320 = 320.1)

# Tidying MIC and Transforming to double(numeric)

# Ps_aeruginosa <- Ps_aeruginosa %>%
#   mutate(
#     across(
# !ends_with("AST"), ~ str_replace_all(., c("<=0.03" = "0.029", "<=1" = "0.09", "<=20" = "19.9",
#                                                      "<=0.06" = "0.059", "<=2" = "1.9",  ">=16" = "16.1",
#                                                      "<=0.12" = "0.119", ">=2" = "2.1",  ">=32" = "32.1",
#                                                      "<=0.25" = "0.249", "<=4" = "3.9",  ">=64" = "64.1",
#                                                      "<=0.5"  = "0.49",  ">=8" = "8.1",  ">=128" = "128.1",
#                                                      ">=0.5"  = "0.51",  "<=10" = "9.9", ">=320" = "320.1"))
#     )
#   )
##############################################################################################################

# #This gives all isolates per year excluding the year 2009. 
# MAY BE VERY  WRONG, THIS CODE GIVED REPEATEDLY THE R BACTEIAL FOR ALL BACTERIA
# 
# combined_files_initial %>% 
#   mutate(
#     Years = as.Date(paste0(year(`Testing Date`), "-01-01"))) %>%   # convert to Date type (Jan 1 of each year)
#   filter(year(Years) != 2009) %>%
#   select(
#     Years, 'Organism Name', 'AN-Other-Expertized':'TZP-Other-Expertized') %>% 
#   pivot_longer(
#     cols = 'AN-Other-Expertized':'TZP-Other-Expertized',                   # antibiotic columns
#     names_to = "Antibiotic",
#     values_to = "Result"
#   ) %>% 
#   filter(Result == "R") %>% 
#   group_by(Years, Result) %>% 
#   #summarise(R_count = n(), .groups = "drop") 
#  
#   summarise(All_Yearly_Isolates = n(), .groups = "drop")
#   
# 
