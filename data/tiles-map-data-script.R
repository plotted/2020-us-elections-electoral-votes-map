# Title: Tile map data of U.S. electoral votes
# 
# Description: Script to generate 2 data frames:
# 1) data for producing the tile map of electoral votes in the U.S.
# 2) table with coordinates to plot U.S. States labels 
#
# Author: Gaston Sanchez
# Date: Jun 2, 2025


# packages
library(tidyverse)


# define grid of 48 (x-axis) by 31 (y-axis)
# this creates a grid with 1488 cells
nx = 48
ny = 31

x = rep(1:nx)
y = rep(1:ny, each = nx)
cells = rep("none", nx * ny)


# Alaska
cells[c(147, 194, 195)] = "Alaska"

# Hawaii
cells[c(57, 58, 104, 151)] = "Hawaii"

# California
cells[c(342:344, 389:392, 436:440, 483:488, 530:535)] = "California"
cells[c(578:582, 625:629, 673:676, 721:724, 769:772)] = "California"
cells[c(817:819, 865:867, 913:915)] = "California"

# Texas
cells[c(253:254, 300:302, 348:351, 395:400, 443:449, 490:497)] = "Texas"
cells[c(540:545, 588:589)] = "Texas"

# Arkansas
cells[c(499, 547:548, 595:597)] = "Arkansas"

# Oklahoma
cells[c(639:641, 686:689)] = "Oklahoma"

# Missouri
cells[c(691:693, 739:741, 787:788, 835:836)] = "Missouri"

# Illinois
cells[c(647:648, 695:696, 743:744, 791:792, 838:840)] = "Illinois"
cells[c(886:888, 934:936, 982:984)] = "Illinois"

# Kentucky
cells[c(650:654, 700:702)] = "Kentucky"

# Indiana
cells[c(746, 794:795, 842:843, 890:891, 938:939, 986:987)] = "Indiana"

# West Virginia
cells[c(705:706, 753:755)] = "West Virginia"

# Virginia
cells[c(608:615, 660:662, 709:710)] = "Virginia"

# District of Columbia
cells[617:619] = "District of Columbia"

# Maryland
cells[c(712:714, 760:762, 805:808)] = "Maryland"

# Delaware
cells[c(669, 716:717)] = "Delaware"

# New Jersey
cells[c(812:813, 858:861, 906:909, 954:957)] = "New Jersey"

# Ohio
cells[c(798:799, 845:848, 893:896, 941:944, 989:992)] = "Ohio"

# Pennsylvania
cells[c(898:904, 946:952, 994:999)] = "Pennsylvania"

# Tennessee
cells[c(503:507, 551:556)] = "Tennessee"

# Arizona
cells[c(632:633, 679:681, 727:729, 775:777)] = "Arizona"

# New Mexico
cells[c(683, 731:732, 779:780)] = "New Mexico"

# Kansas
cells[c(783:785, 831:833)] = "Kansas"

# Oregon
cells[c(1009:1011, 1057:1059, 1105)] = "Oregon"

# Nevada
cells[c(917, 918, 965, 966, 1013, 1014)] = "Nevada"

# Utah
cells[c(872, 873, 920, 921, 968, 969)] = "Utah"

# Colorado
cells[c(875:877, 923:925, 971:973)] = "Colorado"

# Nebraska
cells[c(928:929, 975:977)] = "Nebraska"

# Iowa
cells[c(931:932, 979:980, 1027:1028)] = "Iowa"

# Washington
cells[c(1155:1157, 1203:1205, 1250:1253, 1300:1301)] = "Washington"

# Idaho
cells[c(1111, 1112, 1159, 1207)] = "Idaho"

# Minnesota
cells[c(1122:1124, 1170:1171, 1218:1219, 1266:1268)] = "Minnesota"

# Wyoming
cells[1066:1068] = "Wyoming"

# South Dakota
cells[1070:1072] = "South Dakota"

# Wisconsin
cells[c(1079:1080, 1126:1128, 1174:1176, 1222:1223)] = "Wisconsin"

# Michigan
cells[c(1083:1085, 1131:1134, 1179:1182, 1227:1229, 1273:1274)] = "Michigan"

# New York
cells[c(1049:1050, 1090:1096, 1139:1144, 1188:1192, 1237:1240)] = "New York"
cells[c(1286:1288, 1335:1336)] = "New York"

# Connecticut
cells[c(1100, 1146:1148, 1194:1196)] = "Connecticut"

# Rhode Island
cells[c(1150:1151, 1198:1199)] = "Rhode Island"

# Massachusetts
cells[c(1290:1295, 1338:1342)] = "Massachusetts"

# Montana
cells[1210:1212] = "Montana"

# North Dakota
cells[1214:1216] = "North Dakota"

# Florida
cells[c(36:39, 83:87, 130:135, 177:182, 222:229)] = "Florida"

# Louisiana
cells[c(307:309, 355:357, 403:404)] = "Louisiana"

# Mississippi
cells[c(311:312, 359:360, 407:408)] = "Mississippi"

# Alabama
cells[c(314:316, 362:364, 410:412)] = "Alabama"

# Georgia
cells[c(318:321, 366:369, 414:416, 462:464, 510:511)] = "Georgia"

# South Carolina
cells[c(323:326, 371:375)] = "South Carolina"

# North Carolina
cells[c(466:472, 514:521)] = "North Carolina"

# Vermont
cells[c(1433:1434, 1482)] = "Vermont"

# New Hampshire
cells[c(1436:1437, 1484:1485)] = "New Hampshire"

# Maine
cells[c(1439:1440, 1487:1488)] = "Maine"


# defining cell numbers (one number per cell)
cell_num = matrix(0, nrow = nx, ncol = ny)

for (i in 1:nx) {
  for (j in 1:ny) {
    cell_num[i, j] = i + (nx * (j - 1))
  }
}


# make tile data
dat = data.frame(
  x = rep(1:nx),
  y = rep(1:ny, each = nx),
  cell = as.vector(cell_num),
  state = cells)

# add State 2-letter abbreviation
dat = dat |> 
  mutate(
    # add State 2-letter abbreviation
    abbr = case_when(
      state == "Alabama" ~ "AL",
      state == "Alaska" ~ "AK",
      state == "Arizona" ~ "AZ",
      state == "Arkansas" ~ "AR",
      state == "California" ~ "CA",
      state == "Colorado" ~ "CO",
      state == "Connecticut" ~ "CT",
      state == "Delaware" ~ "DE",
      state == "District of Columbia" ~ "DC",
      state == "Florida" ~ "FL",
      state == "Georgia" ~ "GA",
      state == "Hawaii" ~ "HI",
      state == "Idaho" ~ "ID",
      state == "Illinois" ~ "IL",
      state == "Indiana" ~ "IN",
      state == "Iowa" ~ "IA",
      state == "Kansas" ~ "KS",
      state == "Kentucky" ~ "KY",
      state == "Louisiana" ~ "LA",
      state == "Maine" ~ "ME",
      state == "Maryland" ~ "MD",
      state == "Massachusetts" ~ "MA",
      state == "Michigan" ~ "MI",
      state == "Minnesota" ~ "MN",
      state == "Mississippi" ~ "MS",
      state == "Missouri" ~ "MO",
      state == "Montana" ~ "MT",
      state == "Nebraska" ~ "NE",
      state == "Nevada" ~ "NV",
      state == "New Hampshire" ~ "NH",
      state == "New Jersey" ~ "NJ",
      state == "New Mexico" ~ "NM",
      state == "New York" ~ "NY",
      state == "North Carolina" ~ "NC",
      state == "North Dakota" ~ "ND",
      state == "Ohio" ~ "OH",
      state == "Oklahoma" ~ "OK",
      state == "Oregon" ~ "OR",
      state == "Pennsylvania" ~ "PA",
      state == "Rhode Island" ~ "RI",
      state == "South Carolina" ~ "SC",
      state == "South Dakota" ~ "SD",
      state == "Tennessee" ~ "TN",
      state == "Texas" ~ "TX",
      state == "Utah" ~ "UT",
      state == "Vermont" ~ "VT",
      state == "Virginia" ~ "VA",
      state == "Washington" ~ "WA",
      state == "West Virginia" ~ "WV",
      state == "Wisconsin" ~ "WI",
      state == "Wyoming" ~ "WY",
      state == "none" ~ NA),
    # add State FIPS
    fips = case_when(
      state == "Alabama" ~ "01",
      state == "Alaska" ~ "02",
      state == "Arizona" ~ "04",
      state == "Arkansas" ~ "05",
      state == "California" ~ "06",
      state == "Colorado" ~ "08",
      state == "Connecticut" ~ "10",
      state == "Delaware" ~ "DE",
      state == "District of Columbia" ~ "11",
      state == "Florida" ~ "12",
      state == "Georgia" ~ "13",
      state == "Hawaii" ~ "15",
      state == "Idaho" ~ "16",
      state == "Illinois" ~ "17",
      state == "Indiana" ~ "18",
      state == "Iowa" ~ "19",
      state == "Kansas" ~ "20",
      state == "Kentucky" ~ "21",
      state == "Louisiana" ~ "22",
      state == "Maine" ~ "23",
      state == "Maryland" ~ "24",
      state == "Massachusetts" ~ "25",
      state == "Michigan" ~ "26",
      state == "Minnesota" ~ "27",
      state == "Mississippi" ~ "28",
      state == "Missouri" ~ "29",
      state == "Montana" ~ "30",
      state == "Nebraska" ~ "31",
      state == "Nevada" ~ "32",
      state == "New Hampshire" ~ "33",
      state == "New Jersey" ~ "34",
      state == "New Mexico" ~ "35",
      state == "New York" ~ "36",
      state == "North Carolina" ~ "37",
      state == "North Dakota" ~ "38",
      state == "Ohio" ~ "39",
      state == "Oklahoma" ~ "40",
      state == "Oregon" ~ "41",
      state == "Pennsylvania" ~ "42",
      state == "Rhode Island" ~ "44",
      state == "South Carolina" ~ "45",
      state == "South Dakota" ~ "46",
      state == "Tennessee" ~ "47",
      state == "Texas" ~ "48",
      state == "Utah" ~ "49",
      state == "Vermont" ~ "50",
      state == "Virginia" ~ "51",
      state == "Washington" ~ "53",
      state == "West Virginia" ~ "54",
      state == "Wisconsin" ~ "55",
      state == "Wyoming" ~ "56",
      state == "none" ~ NA))


# quick sanity check
# plot (tile) map of electoral votes
ggplot(dat, aes(x = x, y = y)) +
  geom_tile(color = "white", aes(fill = is.na(abbr))) +
  theme(legend.position = "none")

# export data to CSV file
write_csv(dat, file = "electoral-votes-tiles-map.csv")


# ==============================================================================
# Data table with coordinates of names of states
# ==============================================================================
coords = tribble(
  ~x,  ~y,   ~name,
  2,   3,   "Alaska",
  9,   3.5, "Hawaii",
  3,   13, "Calif.",
  1,   23, "Ore.",
  3,   27, "Wash.",
  6,   27, "Idaho",
  7,   16, "Ariz.",
  10.5, 16.5, "N.M.",
  15,  14.5, "Okla.",
  4.5, 21, "Nev.",
  7.5, 20, "Utah",
  11,  20, "Colo.",
  10,  24, "Wyo.",
  10,  27, "Mont.",
  14,  24, "S.D.",
  14,  27, "N.D.",
  18,  27, "Minn.",
  15,  21, "Neb.",
  15,  17.5, "Kan.",
  13,  10, "Texas",
  19,  8, "La.",
  19,  12.5, "Ark.",
  19,  16, "Mo.",
  18.5, 21, "Iowa",
  22,  24, "Wis.",
  22.5,  8, "Miss.",
  24, 11.5, "Tenn.",
  22.5, 20, "Ill.",
  26,  19, "Ind.",
  30,  19, "Ohio",
  27,  24.5, "Mich.",
  28.5,  14.5, "Ky.",
  26,  8, "Ala.",
  30,  9, "Ga.",
  36,  3, "Fla.",
  36,  7.5, "S.C.",
  36,  10.5, "N.C.",
  33,  17, "W.Va.",
  36.5,  13.5, "Va.",
  42,  12, "D.C.",
  44.5,  13, "Del.",
  40.5, 15.5, "Md.",
  36,  20, "Pa.",
  43,  19, "N.J",
  37,  24, "N.Y.",
  42,  24.5, "Conn.",
  46,  24.5, "R.I.",
  43,  27.5, "Mass.",
  39.5,  30, "Vt.",
  43.5,  30.5, "N.H.",
  46.5,  32, "Maine")

# plot (tile) map of electoral votes
# superimposing labels of States
ggplot() +
  geom_tile(data = dat, 
            aes(x = x, y = y),
            color = "white", fill = dat$party) +
  geom_text(data = coords,
            aes(x = x, y = y, label = name),
            hjust = 0, size = 2.5)

# export table to CSV file
write_csv(coords, file = "states-labels-coords.csv")

