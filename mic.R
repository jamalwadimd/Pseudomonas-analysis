
library(openxlsx)
library(readxl)
library(tidyverse)
library(scales)
library(patchwork)
library(gridExtra)
library(gtsummary)

# List all csv files in the directory

file_list <- list.files(
  path = "C:/Users/User/OneDrive/Private Documents/Research Work/Ongoing Studies/AST database/All years/csv",
  pattern = "\\.csv$",
  full.names = TRUE)

# Creating a list and forcing Patient.ID in the list as character

df_list <- map(
  file_list, ~ read.csv(.x, colClasses = c(Patient.ID = "character")))

# Selecting the necessary columns (Vectors) for the list to data frames to work with.

combined_files <- 
  map_df(df_list, ~ .x) %>% 
    select(
      c("Patient.Name","Patient.ID",
        "Patient.Location","Specimen.Type",
        "Specimen.Source","Testing.Date",
        "Organism.Name"), 
      starts_with("Family..AMINOGLYCOSIDES..."),
      starts_with("Family..BETA.LACTAMS..."),
      starts_with("Family..QUINOLONES..."),
      matches("^AN|ATM|BLA|CAZ|CFM|CIP|CS|ESB|FEP|GM|IPM|LEV|MEM|PIP|TM|TZP"))

# Tidying Patient location

combined_files <- combined_files %>%
  mutate(
    Patient.Location = as.factor(Patient.Location),
    Patient.Location = toupper(Patient.Location),
    Patient.Location = 
      case_when(
        Patient.Location %in% c("CCU","ICU","NICU","Prematurity","micu",
                                "INTERMEDIATE") ~ "ICCU",
        Patient.Location %in% c("OUT","OUUT","OYT","REFFERAL","outr","msf","makka","doctor",
                                "docor","bio") ~ "OUT",
        TRUE ~ "Ward")
  )

# Filling deficient cells in Specimen.Type from Specimen>Source 

combined_files <- combined_files %>%
  mutate(
    Specimen.Type = if_else(
      Specimen.Type == ""|is.na(Specimen.Type),Specimen.Source, Specimen.Type)
  ) %>% 
  select(-Specimen.Source)

# Tidying Specimen.Type

table(combined_files$Specimen.Type)

combined_files <- combined_files %>% 
  mutate(
    Specimen.Type = case_when(
      Specimen.Type %in% c("abdom","abdomen","abdomenal","abdomin","abdominal",
                           "asaad","ascaitic","asciatic","asciatic fluid","asciattic",
                           "ascitec","ascitic","asitic","liver abscess","peritiniol",
                           "peritional","peritoneal","peritoneal fliud","peritoneal fluid",
                           "peritonial","PERITONIAL","peritonial fluid") ~ "Abdomen",
      
      Specimen.Type %in% c("abscess","ABSCESS","absecss","aspirate","aspirate fliud",
                           "aspirated","ASPIRATION","aspitate","aspitate  fliud","aspitre"
                           ,"body","BODY","body .f","body f","BODY F","body fl.",
                           "body fliud","body fliude","body flud","body fluid",
                           "BODY FLUID","body fluids","body fluied","body fluild",
                           "body.f","bodyfluid","bofy f","fliud","fliued","pius",
                           "pous","pu","public","puis","pus","pus-","Pus","PUS","pus-lf",
                           "pusa","puss","puus","s wab","s==swab","sawb","sw2ab","swaab",
                           "swab","SWAB","swab`","swabs","swabswab","swan","swb",
                           "aspirated fluids","aspiration", "fluid","Fluid","FLUID",
                           "fluids","fluied","fluioed","fuid") ~ "Undefined Abscess_Fluid",
      
      Specimen.Type %in% c("atef","b.f","b.f.","b.flied","b.fliued","bal","bAL","Bal",
                           "br-BAL","bronchial wash","lung","pleural","pluaral",
                           "plueral","plural","b.fluied") ~ "Lung_pleura",
      
      Specimen.Type %in% c("bronch","bronchal","broncheal","BRONCHEAL","broncheal wash",
                           "bronchial","soutum","spitum","sptum","spustum","sputm",
                           "sputum","Sputum","SPUTUM","sputum","sputum","spuum",
                           "sputum sputum","teacheal","tracheal") ~ "Bronch_and_Upper",
      
      Specimen.Type %in% c("b;ood","bl","BL","bl;ood","bld","bllod","BLLOD","bllood","blod",
                           "bloiod","bloob","blood","Blood","BLOOD","blood.","blooi","blooo",
                           "bloood","blue","bood","perip","periperal","periphera","peripheral",
                           "PERIPHERAL","periphetral","periphral","periphreal","peripreal",
                           "perpheral","perepheral") ~ "Blood",
      
      Specimen.Type %in% c("cenrtral","central","CENTRAL", "cvc","cvp","cvp line",
                           "jagular","pic line") ~ "CVC",
      
      Specimen.Type %in% c("bone","BONE","bone peice","femoral","femur","syanovial",
                           "synovial") ~ "Bone",
      
      Specimen.Type %in% c("bu cath","by bag","by cath","cath","cathetar","catheter",
                           "catheter foley's","catheter urine","Catheter urine",
                           "CATHETER URINE","cathtar","cathter","CAURI","cauti","CAUTI",
                           "coll","collectar","COLLECTER","collector","condom","cont",
                           "control","floyes","foley's","foleys","foleys of catheter",
                           "folyes","folys","tip","TIP","tip of cath","tip of cather",
                           "tip of cathet","tip of catheter","Tip of catheter",
                           "TIP OF CATHETER","tip of cathter","tipof cath","tipof catheter",
                           "tips") ~ "Urinary_Cath_Bag",
      
      Specimen.Type %in% c("irine","med","medstream","mhmmad","mhmmd","mid",
                           "mid str","mid stream","nepherostomy","nephrostomy",
                           "prostatic","rine","supra","suprapubic","u","uethral",
                           "uine","uirine","ur","UR","uraine","uretheral","urethra",
                           "urethral","urethral swab","urettral","uri ne","uri nr",
                           "urie","URIE","uriine","urin","urinary catheter","urinbe",
                           "urine","Urine","URINE","urine--right","urine+","urine11663566",
                           "urinee","urinemid","urineMID","uriner","urinne",'urinr',
                           "urinre","urinwe","urione","uriune","urne","urone",
                           "urrine","urtheral","urtine","URTINE","uruine","urune",
                           "vboided","vfoided","vided","vioded","viouded","vodid",
                           "voidd urine","voided","VOIDED","voided  urine","voided urin",
                           "voided urine","VOIDED URINE","voided urne","voideed urine",
                           "voied","voied urine","voieded urine","voiided urine","voiuded",
                           "volided","vooided","yrine","vpoided","seemen","semam",
                           "semem","semen","SEMEN","semn","semrn") ~ "Urine",
      
      Specimen.Type %in% c("cervical","h.v.s","high vagainal","high vaginal","HV'S",
                           "HV\"S","hvs","HVS","hyvs","utrine","vagainal","vaginal",
                           "vaginal sawab","vajinal","vulva","vg","VULVA","valvu",
                           "vs") ~ "Vaginal",
      
      Specimen.Type %in% c("csf","CSF","vp shunt") ~ "CSF",
      
      Specimen.Type %in% c("ear","EAR","ear swab","EAR SWAB","ENT","left ear",
                           "LEFT EAR SWAB","lt ear","lt.ear","R.T.Ear","right ear",
                           "rt ear","rt.ear","Rt.ear","RT.ear","RT.EAR",
                           "rt.ear swab") ~ "Ear",
      
      Specimen.Type %in% c("lf thigh","lymphnode", "pericadial","pericardial",
                           "pu(pressure ulcer)","rectum","rt hand","sacral","skin",
                           "skin ulcer","ss","ssi","ssi wound","ssti","SSTI","tieeue",
                           "tissuer","tissuesacral","tissur=e","tissus","tisue",
                           "TISUE","tisuue", "tis","tiss","tisseu","tisseue",
                           "tissu","tissue","TISSUE","tissuea","tongue","tounge",
                           "toungue","tounq","tounque", "wound","wuond","bed sore",
                           "breast","breast milk","d","daiabetic foot","diabetic","diabetic foot",
                           "Diabetic foot","foot diabetic") ~ "SST",
      
      TRUE ~ NA_character_
    )
  )
# Adjusting Testing.Date, and mutating a new Years only column in combined_files, and
## keeping Data for inpatients and excluding 2009

combined_files <- combined_files%>%
  mutate(
    Testing.Date = parse_date_time(Testing.Date, orders = c("mdy", "dmy","ymd")) %>% 
      as.Date(),
    Years = as.Date(paste0(year(Testing.Date), "-01-01"))          # No month or day
    ) %>% 
  filter(Years != 2009,                                                  # Deleting few 2009        
         Patient.Location != "OUT"                                       # Only Inpatients
  ) %>% 
  relocate(Years, .after = Testing.Date)

# Creating new Ps.aeruginosa_Inpatient

Pseudomonas_inpatients <- combined_files %>%
  filter(                                               # Filtering for Ps.aeruginosa
    Organism.Name == "Ps.aeruginosa") %>% 
  mutate(
    Years = as.Date((Years)))

# Years as.Date

class(Pseudomonas_inpatients$Years)
Pseudomonas_inpatients$Years = as.Date(Pseudomonas_inpatients$Years)

# Deleting (TRM = Termination of values by VITEK 2) and changing I as R in MICs.

## All contain data for Experized and Instrument (S, I, R) in the below code.

Pseudomonas_inpatients %>% 
  select(contains(c("Expertized","Instrument"))) %>%          
  map(.,table)

## and removing non-contributory columns

Pseudomonas_inpatients <- Pseudomonas_inpatients %>%
  select(-contains(c("Instrument","CFM", "ESB","BLA"))) %>% 
  mutate(
    across(AN.Amikacin:TZP.Other.Expertized, ~ str_replace_all(.,"TRM",NA_character_)),
    across(ends_with("Expertized"), ~ str_replace_all(., "I", "R")))


# Assigning All other gram-negative bacteria as Yes, else is No (new column = GNB_binomial) 
#   to calculate the ratio of Pseudomonas among all bacteria

combined_files <- combined_files %>% 
  mutate(
    GNB_binomial = case_when(
      Organism.Name %in% c("Achro.denitrificans","Achro.xylosoxidans","Aci.baumannii",
                           "Aci.baumannii cplx","Aci.calcoaceticus","Aci.haemolyticus",
                           "Aci.junii","Aci.lwoffii","Aci.pittii","Aci.radioresistens",
                           "Aci.ursingii","Acinetobacter spp","Burkhol.cepacia",
                           "Burkhol.cepacia gr.","Burkhol.gladioli","Burkhol.pseudomallei","Camp.coli",
                           "Camp.jejuni jejuni","Ced.davisae","Chryse.gleum",
                           "Chryse.indologenes","Citro.amalonaticus","Citro.braakii",
                           "Citro.farmeri","Citro.freundii","Citro.koseri","Citro.sedlakii",
                           "Citro.youngae","Com.testosteroni","Coryn.amycolatum",
                           "Coryn.jeikeium","Coryn.minutissimum","Coryn.urealyticum",
                           "Cro.dub.dublinensis","Cro.sakazakii","Cro.sakazakii group",
                           "Cup.pauculus", "Delftia acidovorans","Eliz.meningosept.",
                           "Ent.aerogenes","Ent.amnigenus","Ent.asburiae","Ent.cloac.dissolvens",
                           "Ent.cloacae",  "Ent.cloacae cloacae","Ent.cloacae complex",
                           "Ent.gergoviae","Esch.coli","Esch.coli O157","Esch.fergusonii",
                           "Esch.hermannii","Ewingella americana","Fran.tularensis",
                           "Fuso.nucleatum","H.influenzae","H.parahaemolyticus","H.parainfluenzae",
                           "Hafnia alvei","K.aerogenes","K.oxytoca","K.pneum.ozaenae",
                           "K.pneum.pneumoniae","K.pneumoniae","Klebsiella spp",
                           "Kluy.cryocrescens","Lacto.acidophilus","Lacto.fermentum",
                           "M.(Bran.)catarrhalis","M.lacunata","Methylobacterium spp",
                           "Moraxella group","Morg.morg.morganii","Morg.morg.sibonii",
                           "Morg.morganii","Proteus hauseri","Proteus mirabilis",
                           "Proteus penneri","Proteus vulgaris","Prov.alcalifaciens",
                           "Prov.rettgeri","Prov.rustigianii","Prov.stuartii",
                           "Ps.aeruginosa","Ps.alcaligenes","Ps.fluorescens","Ps.luteola",
                           "Ps.mendocina","Ps.oleovorans","Ps.oryzihabitans","Ps.putida",
                           "Ps.stutzeri","Pseudomonas","Ser.ficaria","Ser.fonticola",
                           "Ser.liquefaciens","Ser.liquefaciens gr.","Ser.marcescens",
                           "Ser.odorifera","Ser.plymuthica","Ser.rubidaea",
                           "Sphingo.thalpophilum","Sphmon.paucimobilis") ~ "Yes", ## Yes i.e. GNB == 21502
      TRUE ~ "No"                                                     ## No i.e.GPB and non-nososcomial Bacteria == 8510 
    )
  ) %>% 
  relocate(GNB_binomial, .after = Organism.Name) 


###############################################################################
#
#                      ANALYSIS, Figures and Tables  
#
###############################################################################              

# Table 1: Number and Proportions of Patients For Whom P. aeruginosa was isolated 
##########

# Keeping Years as numeric and not date for calculations

combined_files <- combined_files %>% 
  mutate(
    Testing.Date = parse_date_time(Testing.Date, orders = c("mdy","dmy","ymd")) %>% 
      as.Date(),
    Years = year(Testing.Date))  # keeping as numeric, not Date

# Number of Pseudomonas per year

counts_Ps <- Pseudomonas_inpatients %>%  
  mutate(Years = as.numeric(Years)) %>% 
  group_by(Years) %>%
  summarise(Ps_count = n(), .groups = "drop")

# number of GNB isolates per year

counts_GNB <- combined_files %>% 
  filter(GNB_binomial == "Yes") %>% 
  group_by(Years) %>% 
  summarise(GNB_count = n(), .groups = "drop") 

# All Isolates

bact_initial <- combined_files_initial %>% 
  mutate(Years = year(`Testing Date`)) %>% 
  filter(Years>=2010, Years<=2024) %>% 
  group_by(Years) %>% 
  summarise(All_isolates = n(), .groups = "drop") 


# Ratios (Table 1) of P. aeruginosa/GNB per year of the 15 years 
## Then adding them to the major Table (ratios)

ratio <- counts_Ps %>%
  na.omit() %>% 
  left_join(counts_GNB, by = "Years") %>% 
  mutate(
    PsGNB_ratio = round((Ps_count/GNB_count)*100, 3)) %>% 
  left_join(bact_initial, by = "Years") %>% 
  mutate(
    Ps_All = round((Ps_count/All_isolates)*100,3)) 

# Table 1 foot notes
##  Poisson regression to test trend for Ps. aeruginosa "counts" per Years
##  and it gave count drop = -0.02 per year

glm(
  Ps_count ~ Years,
  family = poisson,
  data = ratio) %>% 
  summary()

exp(-0.022395) #= 0.9778539

## Trends for all GNB (Poisson regression )

glm(
  GNB_count ~ Years,
  family = poisson,
  data = ratio) %>% 
  summary()

exp(0.006617) #= 1.006639

## Trend for Ps. aeruginosa/GNB ratio per Years (Poisson regression)

glm(
  PsGNB_ratio ~ Years,
  family = poisson, data = ratio) %>% 
  summary()

exp(0.03926) #= 1.040 

## Trends for All isolates over years

glm(
  All_isolates ~ Years,
  family = poisson, data = ratio) %>% 
  summary()

exp(0.037500) #= 1.04

## Trends for P. aeruginosa/All isolates ratio over years

glm(
  Ps_All ~ Years,
  family = poisson,
  data = ratio) %>% 
  summary()

exp(-0.07695) #= 0.926

## Total GNB Isolates, not including gram-positive and bacteria of endemic nature  = 17216

total_gnb <- combined_files %>%
  filter(
    GNB_binomial == "Yes",
    Patient.Location %in% c("Ward", "ICCU")
  ) %>% 
  nrow()

## Total Ps. aeruginosa Isolates (as in Pseudomonas_inpatients)  = 1728

ps_isolates <- combined_files %>% 
  filter(
    Patient.Location %in% c("Ward", "ICCU"),
    Organism.Name == "Ps.aeruginosa",
    GNB_binomial == "Yes"
  ) %>% 
  nrow()

round(ps_isolates/(total_gnb+ps_isolates),4)*100        ## Pseudomonas Rates/all GNB bacteria = 10%

1728/33418 #= 0.05170866                             # P.aeruginosa ratio from all bacterial isolates

###################
#      Table 2
##################

# Some data from Table 1 foot notes and:
## Counting the cases in ICCU or ward and ratios

Pseudomonas_inpatients %>% 
  count(Patient.Location) %>% 
  mutate(
    Total = sum(n),
    Ratio = round((n/Total)*100,2)
  )

## counting the Specimen Type(Source) and ratios

Pseudomonas_inpatients %>%
  count(Specimen.Type) %>% 
  na.omit() %>% 
  mutate(
    Total = sum(n),
    Ratio = round((n/Total)*100,2)
  )

################################################################################
#                           FIGURES And Plots                                  #
################################################################################
# Figure 1: COMBIND (GNB, Ps.aeruginosa and Trends): The Frequency Distribution of All Gram-Negative Bacteria 
# and Ps. aeruginosa over Fifteen years (2010 - 2024) and P aeruginosa trends.


#--- Common theme for all figures ---
theme_journal <- theme_minimal(base_size = 14, base_family = "sans") +
  theme(
    plot.title      = element_text(face = "bold", hjust = 0.5, size = 15),
    axis.title.x    = element_text(face = "bold", size = 13),
    axis.title.y    = element_text(face = "bold", size = 13),
    axis.text       = element_text(size = 11, color = "black"),
    strip.text      = element_text(face = "bold", size = 12),
    legend.position = "none",
    panel.grid.minor = element_blank(),
    panel.grid.major.x = element_blank(),
    plot.margin = margin(10, 10, 10, 10)
  )

#--- Figure 1A: Frequency of P. aeruginosa vs All GNB ---
freq_plot <- plot_data %>%
  ggplot(aes(x = Years, y = n)) +
  geom_line(linewidth = 1.0) +
  geom_point(size = 2.5) +
  geom_smooth(method = "lm", se = TRUE, linewidth = 1) +
  facet_wrap(~ Group, scales = "free_y", ncol = 1) +
  scale_x_continuous(breaks = seq(2010, 2024, 2)) +
  labs(
    title = expression("A: Frequency Distribution of All Gram-Negative Bacteria and " *italic("P. aeruginosa")),
    x = NULL,
    y = "Number of Isolates"
  ) +
  theme_journal

########### High resolution Editable Figure 1A ################

ggsave(filename = "freq_plot.pdf",
       plot = freq_plot,
       width = 8,
       height = 6,
       units = "in",
       device = cairo_pdf())

#--- Figure 1B: Ratio of P. aeruginosa among all Isolates ---
ratio_plot_ALL <- ratio %>%
  ggplot(aes(x = Years, y = Ps_All)) +
  geom_point(size = 2.5) +
  geom_line(linewidth = 1.0) +
  geom_smooth(method = "lm", se = TRUE, linewidth = 1) +
  scale_x_continuous(breaks = seq(2010, 2024, 2)) +
  labs(
    title = expression("Ratio of " *italic("P. aeruginosa")* " to All Isolates"),
    x = "Years",
    y = "Percentage (%)"
  ) +
  theme_journal

########### High resolution Editable Figure 1B ################

ggsave(filename = "ratio_plot_ALL.pdf",
       plot = ratio_plot_ALL,
       width = 8,
       height = 6,
       units = "in",
       device = cairo_pdf())

#--- Figure 1B: Ratio of P. aeruginosa among all GNB ---
ratio_plot <- Pseudomonas_ratio %>%
  ggplot(aes(x = Years, y = PsGNB_ratio)) +
  geom_point(size = 2.5) +
  geom_line(linewidth = 1.0) +
  geom_smooth(method = "lm", se = TRUE, linewidth = 1) +
  scale_x_continuous(breaks = seq(2010, 2024, 2)) +
  labs(
    title = expression("Ratio of " *italic("P. aeruginosa")* " to All Gram-Negative Bacteria"),
    x = "Years",
    y = "Percentage (%)"
  ) +
  theme_journal

########### High resolution Editable Figure 1B Ratio ################

ggsave(filename = "ratio_plot.pdf",
       plot = ratio_plot,
       width = 8,
       height = 6,
       units = "in",
       device = cairo_pdf())

#--- Combine (vertically stacked, aligned x-axes) ---

combined_Figure1 <- freq_plot/ ratio_plot/ratio_plot_ALL +
  plot_annotation(
    tag_levels = "A",
    title = expression("Figure 1: Trends in "*italic ("P. aeruginosa")*" Frequency and Ratio and The Frequency of Gram-Negative Bacteria (2010–2024)")
  ) &
  theme(plot.title = element_text(size = 16, face = "bold", hjust = 0.5))

combined_Figure1


################################################################################# 

# Figure 2A: Generalization Histogram for Susceptibility together

Fig2A_histogram <- Pseudomonas_inpatients %>%
  select(
    contains("Expertized")) %>% 
  pivot_longer(
    cols = everything(),
    names_to = "Antibiotic",
    values_to = "Result") %>%
  filter(
    !is.na(Result) & Result != "") %>%
  mutate(                                # Rearrange the Antibiotics according to families
    across(everything(),~ na_if(.,"")),
    Antibiotic = factor(
      Antibiotic,
      levels = c("AN.Other.Expertized", "TM.Other.Expertized", "GM.Other.Expertized","ATM.Other.Expertized",
                 "CAZ.Other.Expertized", "FEP.Other.Expertized","IPM.Other.Expertized",
                 "MEM.Other.Expertized", "CIP.Other.Expertized", "LEV.Other.Expertized",
                 "PIP.Other.Expertized", "TZP.Other.Expertized")),
    Antibiotic = recode(Antibiotic,
                        AN.Other.Expertized  = "Amikacin",
                        TM.Other.Expertized  = "Tobramycin",
                        GM.Other.Expertized  = "Gentamicin",
                        ATM.Other.Expertized = "Aztreonam",         # Rename for the Figure
                        CAZ.Other.Expertized = "Ceftazidime",
                        FEP.Other.Expertized = "Cefepime",
                        IPM.Other.Expertized = "Impipenem",
                        MEM.Other.Expertized = "Meropenem",
                        CIP.Other.Expertized = "Ciprofloxacin",
                        LEV.Other.Expertized = "Levofloxacin",
                        PIP.Other.Expertized = "Pipracillin",
                        TZP.Other.Expertized = "Pipraillin-Tazobactam")
  ) %>% 
  drop_na() %>% 
  ggplot(
    aes(x = Antibiotic, fill = Result))+
  geom_bar(position = "fill") +
  scale_y_continuous(labels = scales::percent) +
  labs(
    title = expression("Figure 2A: All tested Isolates of " *italic("P. aeruginosa")* " And Their Susceptibility For Antimicrobials During the Fifteen Years (2010 - 2024)"),
    x = "Anti-Pseudomonal Antimicrobials",
    y = "Number of The Suscptible Isolates")+
  theme_bw()+
  theme(
    legend.position = "bottom")+
  scale_fill_discrete(
    name = "Susceptibility",
    labels = c("S" = "Susceptible",
               "I" = "Intermediate",
               "R" = "Resistant")
  ) 

##########  High Resolution for editing Figure 2 A PDF #################

ggsave(filename = "Fig2A_histogram.pdf",
       plot = Fig2A_histogram,
       width = 8,
       height = 6,
       units = "in",
       device = cairo_pdf())

#########################################################################

# Figure 2B: The Minimum Inhibitory Concentration for the Tested Antimicrobials Against P. aeruginosa (2010 - 2024)

Fig2B_mic <- Pseudomonas_inpatients %>%                       # Reduce numbers to their respected factor category
  select(all_of(
    c("CAZ.Ceftazidime","FEP.Cefepime","ATM.Aztreonam",
      "AN.Amikacin","GM.Gentamicin","TM.Tobramycin",
      "IPM.Imipenem","MEM.Meropenem",
      "LEV.Levofloxacin","CIP.Ciprofloxacin",
      "TZP.Piperacillin.Tazobactam","PIP.Piperacillin"))) %>% 
  mutate(across(everything(.),
                ~ recode(.,
                         "0.12" =  "<=0.12",
                         "0.25" =  "<=0.25",
                         "0.5"  =  "<=0.5",
                         "1"    =  "<=1",
                         "2"    =  "<=2",
                         "4"    =  "<=4",                    # Joining some integers with their counterpart
                         "8"    =  ">=8",
                         "16"   =  ">=16",
                         "32"   =  ">=32",
                         "64"   =  ">=64",
                         "128"  =  ">=128"))
  ) %>% 
  pivot_longer(                                               # Transforming antibacterials to cases
    cols = everything(.),
    names_to = "Antibiotics",       
    values_to = "Results") %>% 
  filter(!is.na(Results), Results != "", Results !="TRM") %>%
  mutate(
    Results = factor(                                         # Organizing factors for visualization
      Results,
      levels = c("<=0.06","0.12","<=0.25","<=0.5","<=4",
                 ">=8",">=16",">=32",">=64",">=128")
    )) %>%
  drop_na() %>% 
  ggplot(aes(x= Results, fill = Results))+
  geom_bar(aes(y = after_stat(prop), group = Antibiotics), position = "dodge")+
  facet_wrap( ~ Antibiotics)+
  labs(
    title = expression("Figure 2B: The Minimum Inhibitory Concentration for the Tested Antimicrobials Against " *italic ("P. aeruginosa ")* "( 2010 - 2024)"),
    y = expression ("Ratio of MIC distribution For Each Antimicrobial Prescribed for " *italic("P. aeruginosa")),
    x = "Minimum Inhibitory Concentration"
  )+
  scale_y_continuous(labels = scales::percent_format())


##########  High Resolution for editing Figure 2B PDF #################

ggsave(filename = "Fig2B_mic.pdf",
       plot = Fig2B_mic,
       width = 8,
       height = 6,
       units = "in",
       device = cairo_pdf())

#########################################################################
Fig2A_histogram
Fig2B_mic

# combining both Figure 2A and 2B
library(patchwork)

Fig2A_histogram / Fig2B_mic + plot_annotation(tag_levels = "A") # if you use / the figures will on top of each other

#####################

# Percent of isolates above the MIC breakpoint, Resistance = "R" EUCAST OR ("R" +"I") CLSI

Pseudomonas_inpatients %>% 
  select(CAZ.Other.Expertized,FEP.Other.Expertized,CIP.Other.Expertized,
         LEV.Other.Expertized,IPM.Other.Expertized,MEM.Other.Expertized,
         ATM.Other.Expertized,AN.Other.Expertized,GM.Other.Expertized,
         TM.Other.Expertized,PIP.Other.Expertized,TZP.Other.Expertized) %>% 
  pivot_longer(
    everything(),
    names_to = "Antibiotics",
    values_to = "Pattern"
  ) %>%
  filter(!is.na(Pattern) & Pattern !="") %>%
  group_by(Antibiotics) %>% 
  summarise(
    total = n(),
    Resistance = sum(Pattern %in% c("R", "I")),    # switch pattern between CLSI (I = R) and EUCAST (I = S dose dependent)
    R_rate = Resistance/total,                     # each antibiotics resistance till here
    .groups = "drop"
  ) %>% 
 mutate(average = mean(R_rate),
        range = max(R_rate) - min (R_rate),
        maximum_rate = max(R_rate),
        minimum_rate = min (R_rate))    # add overall average across antibiotics

###################################################################################

# Figure 3: Combining all P. aeruginosa Susceptibilities to several Antibacterials
##              into one graph with facet_wrap()

hist_SIR <- Pseudomonas_inpatients %>% 
  # reshape antibiotics into long format
  pivot_longer(
    cols = c(AN.Other.Expertized, TM.Other.Expertized, GM.Other.Expertized,
             CAZ.Other.Expertized, IPM.Other.Expertized, MEM.Other.Expertized,
             CIP.Other.Expertized, LEV.Other.Expertized, PIP.Other.Expertized,
             TZP.Other.Expertized),
    names_to = "Antibiotic",
    values_to = "Result"
  ) %>%
  mutate(                                # Rearrange the Antibiotics according to families
    Antibiotic = factor(
      Antibiotic,
      levels = c("AN.Other.Expertized", "TM.Other.Expertized", "GM.Other.Expertized",
                 "CAZ.Other.Expertized", "IPM.Other.Expertized", "MEM.Other.Expertized",
                 "CIP.Other.Expertized", "LEV.Other.Expertized", "PIP.Other.Expertized",
                 "TZP.Other.Expertized")
      ),
    Years = as.numeric(Years)
  ) %>% 
  filter(Result %in% c("S","I","R")) %>% 
  ggplot(
    aes(x = Years, fill = Result))+
  geom_bar(position = "fill")+
  facet_wrap(~ Antibiotic,
             labeller = labeller(                      # Rename for the Figure
               Antibiotic = c(
                 AN.Other.Expertized  = "Amikacin",
                 TM.Other.Expertized  = "Tobramycin",
                 GM.Other.Expertized  = "Gentmicin",
                 CAZ.Other.Expertized = "Ceftazidime",
                 IPM.Other.Expertized = "Impipenem",
                 MEM.Other.Expertized = "Meropenem",
                 CIP.Other.Expertized = "Ciprofloxacin",
                 LEV.Other.Expertized = "Levofloxacin",
                 PIP.Other.Expertized = "Pipracillin",
                 TZP.Other.Expertized = "Pipraillin-Tazobactam")
             ))+
  scale_y_continuous(breaks = seq(0, 1,0.1))+
  scale_x_continuous(breaks = seq(2010, 2024, 5))+
  labs(
    x = "Years",
    y = "Susceptibility Patterns"
  )+
  theme_minimal()+
  theme(
    legend.position = c(0.8, 0.15))+
  scale_fill_discrete(
    name = "Susceptibility",
    labels = c("R" = "Resistant",
               "I" = "Intermediate",
               "S" = "Susceptible"
    ))

hist_SIR


##########  High Resolution for editing Figure 3 hist PDF #################

ggsave(filename = "hist_SIR.pdf",
       plot = hist_SIR,
       width = 8,
       height = 6,
       units = "in",
       device = cairo_pdf())

#########################################################################

################################################################################

## For the Figure 3 table P-value for trends including the intercept

trend_results <- Pseudomonas_inpatients %>%
  mutate(Years = as.numeric(Years)) %>% 
  pivot_longer(
    cols = ends_with("Expertized"),
    names_to = "Antibiotic",
    values_to = "Result"
  ) %>%
  filter(Result %in% c("S", "I", "R")) %>%
  filter(!is.na(Result), Result != "", Result !="TRM") %>%
  mutate(Resistant = ifelse(Result == "R", 1, 0)) %>%
  group_by(Years, Antibiotic) %>%
  summarise(
    R_count = sum(Resistant),
    N_total = n(),
    .groups = "drop"
  ) %>%
  group_by(Antibiotic) %>%
  do({
    model <- glm(cbind(R_count, N_total - R_count) ~ Years,
                 family = binomial, data = .)
    broom::tidy(model)
  })

# Figure 3B Resistance trends without the intercept

trend_results_no_intercept <- trend_results %>%            
  filter(term == "Years") %>%
  select(Antibiotic, estimate, std.error, p.value) %>% 
  mutate(across(
    where(is.numeric), ~ round(.x,3))
  )

# Figure 3: CRPa ratios/ Antibiotic and Years c (Resistance curves over years) And Resistance Trends (Table).

CRPa_ggplot <- Pseudomonas_inpatients %>%
  select(Years, Organism.Name,
         AN.Other.Expertized,GM.Other.Expertized,ATM.Other.Expertized,
         TM.Other.Expertized, CAZ.Other.Expertized, FEP.Other.Expertized,
         IPM.Other.Expertized,MEM.Other.Expertized,CIP.Other.Expertized,
         LEV.Other.Expertized,PIP.Other.Expertized,TZP.Other.Expertized) %>%
  pivot_longer(
    cols = ends_with("Expertized"),
    names_to = "Antibiotic",
    values_to = "Result"
  ) %>%
  mutate(
    Antibiotic = factor(
      Antibiotic,
      levels = c("AN.Other.Expertized","TM.Other.Expertized", "GM.Other.Expertized",
                 "ATM.Other.Expertized","CAZ.Other.Expertized","FEP.Other.Expertized",
                 "IPM.Other.Expertized", "MEM.Other.Expertized","CIP.Other.Expertized",
                 "LEV.Other.Expertized", "PIP.Other.Expertized","TZP.Other.Expertized"))
  ) %>%
  filter(Result %in% c("S","I","R")) %>%
  mutate(Years = as.numeric(Years)) %>% 
  group_by(Years, Antibiotic, Result) %>%
  summarise(n = n(), .groups = "drop_last") %>%
  mutate(total = sum(n)) %>%
  ungroup() %>%
  mutate(rate = n / total * 100) %>%
  filter(Result == "R") %>%    # 👈 keep only Resistance
  mutate(Antibiotic = recode(Antibiotic,
                             AN.Other.Expertized  = "Amikacin",
                             TM.Other.Expertized  = "Tobramycin",
                             GM.Other.Expertized  = "Gentmicin",
                             ATM.Other.Expertized = "Aztreonam",
                             CAZ.Other.Expertized = "Ceftazidime",
                             FEP.Other.Expertized = "Cefepime",
                             IPM.Other.Expertized = "Imipenem",
                             MEM.Other.Expertized = "Meropenem",
                             CIP.Other.Expertized = "Ciprofloxacin",
                             LEV.Other.Expertized = "Levofloxacin",
                             PIP.Other.Expertized = "Pipracillin",
                             TZP.Other.Expertized = "Pipraillin-Tazobactam")
  ) %>%
  ggplot(
    aes(x = Years, y = rate)) +
  geom_line(linewidth = 1.2, color = "red") +        # only resistant, so color fixed
  geom_smooth(se = FALSE, method = lm, na.rm = TRUE,
              linewidth = 1.2, color = "navy") +    # regression line on R
  facet_wrap(~Antibiotic) +
  labs(
    x = "Year",
    y = "Resistance (%)"
  ) +
  theme_minimal()

##########  High Resolution for editing Figure 3 hist PDF #################

ggsave(filename = "CRPa_ggplot.pdf",
       plot = CRPa_ggplot,
       width = 8,
       height = 6,
       units = "in",
       device = cairo_pdf())

#########################################################################

############
# Table: Creating a tibble to estimate the Resistance odds, the Effect size(RR) and 95% C.I. to Joint Figure 3.
## double were calculated from : 'glm.slope_Antimicrobials'
glm.slope_Antimicrobials <- 
  tibble(
    slope = c(-0.035,-0.1,-0.086, -0.106,-0.037,-0.043,0.008,0.008,-0.04,0.035,-0.077,-0.05),
    Std.Error = c(0.016,0.015,0.015,0.04,0.015,0.015,0.013,0.015,0.013,0.034,0.017,0.015),
    Antibiotics = c("Amikacin","Tobramycin","Gentamicin","Aztreonam","Ceftazidime",
                    "Cefepime","Imipenem","Meropenem","Ciprofloxacin","Levofloxacin",
                    "Pipracillin","Pipracillin-Tazobactam")
  ) %>% 
  mutate(
    RR = round(exp(slope),3),
    CI_low  = round(exp(slope - 1.96*Std.Error),3),
    CI_high = round(exp(slope + 1.96*Std.Error),3),
    P_value = round(2*(1-pnorm(abs(slope/Std.Error))),4)
  ) %>% 
  relocate(Antibiotics, .before = slope)

#####   Above table as PDF #####
install.packages("gt")
library(gt)

# explicitly create tbl_gt first

tbl_gt <- glm.slope_Antimicrobials %>%
  gt()

install.packages("webshot2")
webshot2::install_chromium()

# Now save the table as PDF

gtsave(
  data = tbl_gt,
  filename = "glm.slope_Antimicrobials.pdf"
)

################################################################################

# Join the glm.slope_Antimicrobials & Figure 3  as a Table

# Example: turn your slope table into a grob
slope_table <- glm.slope_Antimicrobials %>%
  dplyr::select(Antibiotics, slope, Std.Error, RR, CI_low, CI_high, P_value) %>%
  tableGrob(rows = NULL)   # removes row names

# Wrap the table grob into a patchwork object
slope_table_plot <- wrap_elements(slope_table)

# Combine with your ggplot
final_plot <- (hist_SIR + CRPa_ggplot + slope_table_plot)+
  plot_annotation(
    title = expression("Figure 3: " *italic("P. aeruginosa")* " (A) Susceptibility, (B) Resistance trends, (C) Effect Size, Rate Ratio (RR), And Significance (2010 -2024)" ),
    tag_levels = "A"
    ) &
  theme(
  plot.tag.position = "bottom"
)

final_plot

################################################################################
 
# Figure 4

################################################################################

# CRPa for Years and for all Antibiotic

r_total <- Pseudomonas_inpatients %>% 
  mutate(Years = year(Testing.Date)) %>% 
  select(Years, Organism.Name,AN.Other.Expertized, TM.Other.Expertized, GM.Other.Expertized,
         CAZ.Other.Expertized, IPM.Other.Expertized, MEM.Other.Expertized,
         CIP.Other.Expertized, LEV.Other.Expertized, PIP.Other.Expertized,
         TZP.Other.Expertized) %>% 
  pivot_longer(
    cols = AN.Other.Expertized:TZP.Other.Expertized,                    # only antibiotic columns
    names_to = "Antibiotic",
    values_to = "Result"
  ) %>% 
  filter(Result == "R") %>% 
  group_by(Years, Antibiotic) %>%                                       # I added Antibiotic in group_by
  summarise(R_count = n(), .groups = "drop") %>% 
  pivot_wider(names_from = "Antibiotic",                                  # I added pivot_wider is added to show R count
              values_from = "R_count") %>%                                ## s per Antibiotic and Year
  print(n=21, width=Inf)

# Calculate total sum of resistance in Beta = 553 (32.0%)

Pseudomonas_inpatients %>%  
  select(starts_with("Family..BETA.LACTAMS")) %>%
  mutate(
    across(
      starts_with("Family..BETA.LACTAMS"), 
      ~ if_else(str_detect(.x, paste(patterns, collapse = "|")), 1, 0)  ),
    resistance_count = rowSums(across(starts_with("Family..BETA.LACTAMS")), na.rm = TRUE) ) %>%
  summarise(
    total_resistance = sum(resistance_count, na.rm = TRUE),
    total_isolates =n(),
    Total_resistance_ratio = total_resistance/total_isolates)

################################################################################

# FOR FIGURE 4: Looking for Carbapenems resistance in all BETA.LACTAMs , down

Pseudomonas_inpatients %>% 
  select(contains("BETA.LACTAMS")) %>% 
  map(., table)

# Cleaning BETA.LACTAMS columns strings from words and coding where carbapenem-resistant 
## as 1 then transforming any interger to 1,
###then summing up all rows in sum_all  Carpabenems resistance

CRPa <- Pseudomonas_inpatients %>%
  select(Years, contains("BETA.LACTAMS")) %>%
  mutate(                                              # convert numeric year to Date
    Years = as.Date(paste0(Years, "-01-01"))) %>%      
  mutate(                                              #coding where carbapenem-resistant as integer (1)
    across(everything(), ~ str_replace_all(.x, c("CARBAPENEMS" = "1","CARBAPENEMASE" = "1")))  
  ) %>%
  mutate(across(everything(), ~ str_replace_all(.x, "\\D", ""))) %>%      # keep only digits, cleaning words
  mutate(across(everything(), ~ na_if(.x, ""))) %>%                       # empty string to NA
  mutate(across(everything(), as.integer)) %>%                            # keep integer
  select(where(~ any(.x == 1, na.rm = TRUE))) %>%                         # select cells with integer
  mutate(
    sum_all = rowSums(across(contains("BETA.LACTAMS")), na.rm = TRUE)     # sum all rows
  ) %>% 
  mutate(sum_all = if_else(sum_all> 0,1, 0),
         Years = Pseudomonas_inpatients$Years)                                # adding year

## Figure 4: Calculating the yearly CRPa and visualization by a plot ()geom_smooth(lm and se))
############

CRPa_Figure <- CRPa %>% 
  select(Years, sum_all) %>% 
  ggplot(aes(x = Years, y = sum_all))+
  geom_smooth(se = TRUE, na.rm = TRUE, method = "lm")+
  labs(
    title = expression("Figure 4: Trends of the Carbapenem-Resistant " *italic("P. aeruginosa")* " (CRPa) over Fifteen Years (2010 - 2024)"),
    y = "Frequency of the CRPa", 
  )+
  theme_minimal()+
  scale_x_continuous(n.breaks = 15)

######## Creating Figure 4 PDF 

ggsave(filename = "CRPa_Figure.pdf",
       plot = CRPa_Figure,
       width = 8,
       height = 6,
       units = "in",
       device = cairo_pdf())

# calculating the count and ratio for the CRPa as numeric


totalCRPa <- sum(CRPa$sum_all, na.rm = TRUE)
total_Ps <- 1728
CRPa_rate <- round((totalCRPa/total_Ps)*100,2)



Pseudomonas_inpatients %>%
  select(Years, contains("BETA.LACTAMS")) %>%
  mutate(
    Years = as.Date(paste0(Years, "-01-01")),
    across(
      everything(),
      ~ str_replace_all(.x, c("CARBAPENEMS" = "1", "CARBAPENEMASE" = "1"))
    ),
    across(everything(), ~ str_replace_all(.x, "\\D", "")),
    across(everything(), ~ na_if(.x, "")),
    across(everything(), as.integer)
  ) %>%
  mutate(
    sum_all = rowSums(across(contains("BETA.LACTAMS")), na.rm = TRUE),
    sum_all = if_else(sum_all > 0, 1L, 0L)  # 1 if resistant to any, else 0
  ) #%>%
# group_by(Years) %>%
# summarise(
#   CRPa = sum(sum_all, na.rm = TRUE)       # <-- total CRPa per year (colSums equivalent)
# ) %>%
# ungroup()

# then the total and ratio of CRPa



#################################################################################
# The Overall trends for resistance behavior in Pseudomonas
# Assessing the absolute Effect size for the trends based on the slope in glm

model <- Pseudomonas_inpatients %>%
  pivot_longer(
    cols = ends_with("Expertized"),
    names_to = "Antibiotic",
    values_to = "Result"
  ) %>%
  filter(Result %in% c("S", "I", "R")) %>%
  mutate(Resistant = ifelse(Result == "R", 1, 0)) %>% 
  glm(Resistant ~ Years,
      family = binomial,
      data = .)
summary(model)

# Transforming slope into a measurably clear effect size

install.packages("effectsize")
library(effectsize)
library(broom)

effectsize(model, ci = 0.95)  #log odds of the years.
tidy(model, exponentiate = TRUE, conf.int = TRUE) #OR

exp(-0.15)   # = 0.860708   (Years|std. Coef = -0.15| 95% C.I. = [-0.18, -0.12])
1-exp(-0.15) # = 0.139292 ≈ 0.14
# Means: Each additional year is associated with about a 14 % decrease in the odds of resistance.
exp(-0.18);exp(-0.12) # for 95% Lower CI = exp(-0.18) ≈ 0.84, 
# Upper CI = exp(-0.12) ≈ 0.89   = (OR 0.86, 95% CI 0.84–0.89, p < 0.001)


library(janitor)


# Detecting other mechanisms (other than CRPa) of resistance (in general)

Pseudomonas_inpatients %>% 
  select(starts_with("Family")) %>%
  select(-c(Family..AMINOGLYCOSIDES...8,Family..AMINOGLYCOSIDES...9,Family..AMINOGLYCOSIDES...11,
            Family..AMINOGLYCOSIDES...17,Family..AMINOGLYCOSIDES...22, Family..AMINOGLYCOSIDES...23,
            Family..AMINOGLYCOSIDES...24,Family..AMINOGLYCOSIDES...25,Family..AMINOGLYCOSIDES...26,
            Family..AMINOGLYCOSIDES...27,Family..AMINOGLYCOSIDES...28,Family..AMINOGLYCOSIDES...29,
            Family..AMINOGLYCOSIDES...30,Family..AMINOGLYCOSIDES...31,Family..AMINOGLYCOSIDES...32,
            Family..BETA.LACTAMS...33,Family..BETA.LACTAMS...34,Family..BETA.LACTAMS...39,
            Family..BETA.LACTAMS...40,Family..BETA.LACTAMS...42,Family..BETA.LACTAMS...55,
            Family..BETA.LACTAMS...56,Family..BETA.LACTAMS...57,Family..BETA.LACTAMS...58,
            Family..BETA.LACTAMS...59,Family..BETA.LACTAMS...61,Family..BETA.LACTAMS...62,
            Family..BETA.LACTAMS...63,Family..BETA.LACTAMS...64,Family..BETA.LACTAMS...65,
            Family..BETA.LACTAMS...66,Family..BETA.LACTAMS...67,Family..BETA.LACTAMS...68,
            Family..BETA.LACTAMS...69,Family..BETA.LACTAMS...70,Family..BETA.LACTAMS...71,
            Family..BETA.LACTAMS...72,Family..BETA.LACTAMS...73,Family..QUINOLONES...74,
            Family..QUINOLONES...76,Family..AMINOGLYCOSIDES...33,Family..QUINOLONES...65,
            Family..QUINOLONES...66,Family..AMINOGLYCOSIDES...34,Family..QUINOLONES...57,
            Family..QUINOLONES...58,Family..BETA.LACTAMS...27,Family..BETA.LACTAMS...28,
            Family..BETA.LACTAMS...29,Family..BETA.LACTAMS...30,Family..BETA.LACTAMS...31,
            Family..BETA.LACTAMS...32,Family..QUINOLONES...61,Family..QUINOLONES...62)) %>%
  #map(., table)
  # select(contains("BETA.LACTAMS")) %>%
  # pivot_longer(
  #   cols = contains("BETA.LACTAMS"),
  #   names_to = "Family_BETALACTAMS",
  #   values_to = "Results_Beta"
  # ) %>% table() %>%
  # colSums()
# select(contains("AMINOGLYCOSIDES")) %>%
#   pivot_longer(
#     cols = contains("AMINOGLYCOSIDES"),
#     names_to = "Aminoglycosides",
#     values_to = "Result_Amino"
#   ) %>% table()%>%
#   colSums()
select(contains("QUINOLONES")) %>%
pivot_longer(
  cols = contains("QUINOLONES"),
  names_to = "Quinolones",
  values_to = "Result_Quino"
) %>% table()%>%
colSums()


################################################################################
#                                                                              #
#                          May Be Needed Codes Later                           #   
#                                                                              #
################################################################################

  
# Extracting resistant Pseudomonas aeruginosa for the "Family..BETA.LACTAMS" vectors

# Extracting CRPa for the "Family..BETA.LACTAMS" vectors

Pseudomonas_inpatients %>%
  select(starts_with("Family..BETA.LACTAMS")) %>% 
  map(~ as.data.frame(table(.)))

#   
Pseudomonas_inpatients %>% 
  select(contains("BETA.LACTAMS")) %>% 
  pivot_longer(everything(),
               names_to = "Antibiotic_Family",
               values_to = "Result"
  ) %>% 
  count(Antibiotic_Family, Result, name = "N") %>% 
  mutate(
    Antibiotic_Family = case_when(
      Antibiotic_Family %in% c("CARBAPENEMS","CARBAPENEMASE", "ACQ PASE+ R CARBAPENEMS (IMPERMEABILITY)",
                               "ESBL + R CARBAPENEMS (IMPER)","HL CASE + R CARBAPENEMS (IMPER)",
                               "HIGH LEVEL R + R CARBAPENEMS (IMPER)","RESISTANT CARBAPENEMS (IMPERMEABILITY)",
                               "EFFLUX (MexAB)","ESBL (CLAVULANATE INHIBITED)","CARBAPENEMASE (METALLO- OR OXA)",
                               "ESBL + R CARBAPENEMS (IMPER)",""
      ) ~ "Yes",
      TRUE ~ Antibiotic_Family
    )
  ) %>% 
  filter(                                          # filtering the only bacteria with CRPa tota1 = 257
    Result %in% c(
      "ESBL + R CARBAPENEMS (IMPER)","ESBL (CLAVULANATE INHIBITED)",
      "ESBL + R CARBAPENEMS (IMPER)"
    )
  ) %>%
  # filter(Result %in% c("CARBAPENEMS","CARBAPENEMASE", "ACQ PASE+ R CARBAPENEMS (IMPERMEABILITY)",
  #                      "ESBL + R CARBAPENEMS (IMPER)","HL CASE + R CARBAPENEMS (IMPER)",
  #                      "HIGH LEVEL R + R CARBAPENEMS (IMPER)","RESISTANT CARBAPENEMS (IMPERMEABILITY)",
  #                      "CARBAPENEMASE (METALLO- OR OXA)",
  #                      "ESBL + R CARBAPENEMS (IMPER)")
  # ) %>%
  # filter(Result == "CARBAPENEMASE (METALLO- OR OXA)"
  # ) %>%
  # filter(Result == "HIGH LEVEL CEPHALOSPORINASE"
  # ) %>% 
  # filter(Result == "EFFLUX (MexAB)"
  # ) %>% 
  #print(n = 50) %>% 
  summarise(total = sum(N))
  

# Extracting ESBL-Pseudomonas aeruginosa for the "Family..BETA.LACTAMS" vectors

Pseudomonas_inpatients %>% 
  select(contains("BETA.LACTAMS")) %>% 
  pivot_longer(everything(),
               names_to = "Antibiotic_Family",
               values_to = "Result2"
  ) %>% 
  count(Antibiotic_Family, Result2, name = "N") %>% 
  mutate(
    Antibiotic_Family = case_when(
      Antibiotic_Family %in% c("ESBL (CLAVULANATE INHIBITED)","ESBL + R CARBAPENEMS (IMPER)") ~ "Yes",
      TRUE ~ Antibiotic_Family
    )
  ) %>% 
  filter(                                             # filtering the only bacteria with CRPa total = 113, Clavulonate inhibited = 41
    Result2 %in% c("ESBL (CLAVULANATE INHIBITED)","ESBL + R CARBAPENEMS (IMPER)")
  )

# Extracting Quinolones-R Pseudomonas aeruginosa for the "Family..QUINOLONES" vectors

Pseudomonas_inpatients %>% 
  select(contains("QUINOLONES")) %>% 
  map( ~ table(.))

# Extracting Quinolones_R-Pseudomonas aeruginosa for the "Family..QUINOLONES" vectors

Pseudomonas_inpatients %>% 
  select(contains("QUINOLONES")) %>% 
  pivot_longer(everything(),
               names_to = "Quinolones_R",
               values_to = "Result3"
  ) %>% 
  count(Quinolones_R, Result3, name = "N") %>% 
  mutate(
    Quinolones_R = case_when(
      Quinolones_R %in% c("RESISTANT","WILD") ~ "Yes",
      TRUE ~ Quinolones_R
    )) %>%
  filter(Result3 %in% c("RESISTANT","WILD"))

# Extracting AMINOGLYCOSIDES-R Pseudomonas aeruginosa for the "Family..AMINOGLYCOSIDES" vectors

Pseudomonas_inpatients %>% 
  select(contains("AMINOGLYCOSIDES")) %>% 
  map( ~ table(.))

# Extracting AMINOGLYCOSIDES_R-Pseudomonas aeruginosa for the "Family..AMINOGLYCOSIDES" vectors 

Pseudomonas_inpatients %>% 
  select(contains("AMINOGLYCOSIDES")) %>%
  pivot_longer(everything(),
               names_to = "Aminoantimicrobial",
               values_to = "Result4"
  ) %>%
  count(Aminoantimicrobial, Result4, name = "n") %>% 
  mutate(
    Aminoantimicrobial = case_when(
      Aminoantimicrobial %in% c("RESISTANT (GEN NET)","RESISTANT (GEN)","RESISTANT (TOB GEN NET)",
                                "RESISTANT (GEN NET AMI TOB)","RESISTANT (TOB GEN)","RESISTANT (GEN NET AMI)",
                                "RESISTANT (TOB NET AMI)","WILD") ~ "Yes",
      TRUE ~ Aminoantimicrobial
    )) %>% 
  filter(Result4 %in% c("RESISTANT (GEN NET)","RESISTANT (GEN)","RESISTANT (TOB GEN NET)",
                        "RESISTANT (GEN NET AMI TOB)","RESISTANT (TOB GEN)","RESISTANT (GEN NET AMI)",
                        "RESISTANT (TOB NET AMI)","WILD")) %>% 
  print(n=23)



clean_names(Pseudomonas_inpatients)

family_aminoglycosides_12



# Plotting A single antimicrobial Susceptibility in Ps. aeruginosa versus Years #

### Amikacin

Pseudomonas_inpatients %>%
  mutate(
    Years = as.Date(Years)) %>%
  filter(
    AN.Other.Expertized %in% c("S","I","R")
  ) %>%
  ggplot(aes(x = Years, fill = AN.Other.Expertized))+
  geom_bar(position = "fill")+
  scale_x_continuous(breaks = unique(Pseudomonas_inpatients$Years))+
  scale_y_continuous(breaks = seq(0,1,0.1))+
  labs(
    title = "Susceptibility of Ps. aeruginosa to Amikacin over the last Fifteen Years",
    y = " The percent of Susceptible, Intermediate and Resistant Pseudomonas "
  )


### Ceftazidime

Pseudomonas_inpatients %>%
  mutate(
    Years = as.Date(Years)) %>%
  filter(
    CAZ.Other.Expertized %in% c("S","I","R")
  ) %>%
  ggplot(aes(x = Years, fill = CAZ.Other.Expertized))+
  geom_bar(position = "fill")+
  scale_x_continuous(breaks = unique(Pseudomonas_inpatients$Years))+
  scale_y_continuous(breaks = seq(0,1,0.1))+
  labs(
    title = "Susceptibility of Ps. aeruginosa to Ceftazidime over the last Fifteen Years",
    y = " The percent of Susceptible, Intermediate and Resistant Pseudomonas "
  )

### CIP.Other.Expertized
Pseudomonas_inpatients %>%
  mutate(
    Years = as.Date(Years)) %>%
  filter(
    CIP.Other.Expertized %in% c("S","I","R")
  ) %>%
  ggplot(aes(x = Years, fill = CIP.Other.Expertized))+
  geom_bar(position = "fill")+
  scale_x_continuous(breaks = unique(Pseudomonas_inpatients$Years))+
  scale_y_continuous(breaks = seq(0,1,0.1))+
  labs(
    title = "Susceptibility of Ps. aeruginosa to Ceftazidime over the last Fifteen Years",
    y = " The percent of Susceptible, Intermediate and Resistant Pseudomonas "
  )

### CS.Other.Expertized, not included, it was not tested by the tube broth method

Pseudomonas_inpatients %>%
  mutate(
    Years = as.Date(Years)) %>%
  filter(
    CS.Other.Expertized %in% c("S","I","R")
  ) %>%
  ggplot(aes(x = Years, fill = CS.Other.Expertized))+
  geom_bar(position = "fill")+
  scale_x_continuous(breaks = unique(Pseudomonas_inpatients$Years))+
  scale_y_continuous(breaks = seq(0,1,0.1))+
  labs(
    title = "Susceptibility of Ps. aeruginosa to Colistin over the last Fifteen Years",
    y = " The percent of Susceptible, Intermediate and Resistant Pseudomonas "
  )

### FEP.Other.Expertized

Pseudomonas_inpatients %>%
  mutate(
    Years = as.Date(Years)) %>%
  filter(
    FEP.Other.Expertized %in% c("S","I","R")
  ) %>%
  ggplot(aes(x = Years, fill = FEP.Other.Expertized))+
  geom_bar(position = "fill")+
  scale_x_continuous(breaks = unique(Pseudomonas_inpatients$Years))+
  scale_y_continuous(breaks = seq(0,1,0.1))+
  labs(
    title = "Susceptibility of Ps. aeruginosa to Ceftazidime over the last Fifteen Years",
    y = " The percent of Susceptible, Intermediate and Resistant Pseudomonas "
  )

### GM.Other.Expertized

Pseudomonas_inpatients %>%
  mutate(
    Years = as.Date(Years)) %>%
  filter(
    GM.Other.Expertized %in% c("S","I","R")
  ) %>%
  ggplot(aes(x = Years, fill = GM.Other.Expertized))+
  geom_bar(position = "fill")+
  scale_x_continuous(breaks = unique(Pseudomonas_inpatients$Years))+
  scale_y_continuous(breaks = seq(0,1,0.1))+
  labs(
    title = "Susceptibility of P. aeruginosa to Ceftazidime over the last Fifteen Years",
    y = " The percent of Susceptible, Intermediate and Resistant Pseudomonas "
  )

### IPM.Other.Expertized

Pseudomonas_inpatients %>%
  mutate(
    Years = as.Date(Years)) %>%
  filter(
    IPM.Other.Expertized %in% c("S","I","R")
  ) %>%
  ggplot(aes(x = Years, fill = IPM.Other.Expertized))+
  geom_bar(position = "fill")+
  scale_x_continuous(breaks = unique(Pseudomonas_inpatients$Years))+
  scale_y_continuous(breaks = seq(0,1,0.1))+
  labs(
    title = "Susceptibility of Ps. aeruginosa to Ceftazidime over the last Fifteen Years",
    y = " The percent of Susceptible, Intermediate and Resistant Pseudomonas "
  )

# ## MEM.Other.Expertized

Pseudomonas_inpatients %>%
  mutate(
    Years = as.Date(Years)) %>%
  filter(
    MEM.Other.Expertized %in% c("S","I","R")
  ) %>%
  ggplot(aes(x = Years, fill = MEM.Other.Expertized))+
  geom_bar(position = "fill")+
  scale_x_continuous(breaks = unique(Pseudomonas_inpatients$Years))+
  scale_y_continuous(breaks = seq(0,1,0.1))+
  labs(
    title = "Susceptibility of P. aeruginosa to Ceftazidime over the last Fifteen Years",
    y = " The percent of Susceptible, Intermediate and Resistant Pseudomonas "
  )

### PIP.Other.Expertized

Pseudomonas_inpatients %>%
  mutate(
    Years = as.Date(Years)) %>%
  filter(
    PIP.Other.Expertized %in% c("S","I","R")
  ) %>%
  ggplot(aes(x = Years, fill = PIP.Other.Expertized))+
  geom_bar(position = "fill")+
  scale_x_continuous(breaks = unique(Pseudomonas_inpatients$Years))+
  scale_y_continuous(breaks = seq(0,1,0.1))+
  labs(
    title = "Susceptibility of Ps. aeruginosa to Ceftazidime over the last Fifteen Years",
    y = " The percent of Susceptible, Intermediate and Resistant Pseudomonas "
  )

### TZP.Other.Expertized

Pseudomonas_inpatients %>%
  mutate(
    Years = as.Date(Years)) %>%
  filter(
    TZP.Other.Expertized %in% c("S","I","R")
  ) %>%
  ggplot(aes(x = Years, fill = TZP.Other.Expertized))+
  geom_bar(position = "fill")+
  scale_x_continuous(breaks = unique(Pseudomonas_inpatients$Years))+
  scale_y_continuous(breaks = seq(0,1,0.1))+
  labs(
    title = "Susceptibility of Ps. aeruginosa to Ceftazidime over the last Fifteen Years",
    y = " The percent of Susceptible, Intermediate and Resistant Pseudomonas "
  )

  
         
         