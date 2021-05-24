# Function Creates html tables for ER Monitoring Report

createhtmlTable4_2 <- function(){

kbl(Table4_2) %>%
  kable_styling(bootstrap_options = c("striped", "condensed", "hover", full_width = F, position = "left")) %>%
  column_spec(2, width = "20em") %>%
  save_kable(file = "Table4_2.html", self_contained = T) }

createhtmlTable4_3 <- function(){

kbl(Table4_3) %>%
  kable_styling(bootstrap_options = c("striped", "condensed", "hover", full_width = F, position = "left")) %>%
  column_spec(1, width = "20em") %>%
  save_kable(file = "Table4_3.html", self_contained = T) }

createhtmlTable5_2_2 <- function(){
kbl(Table5_2_2) %>%
  kable_styling(bootstrap_options = c("striped", "condensed", "hover", full_width = F, position = "left")) %>%
  column_spec(1, width = "20em") %>%
  column_spec(2, width = "20em") %>%
  save_kable(file = "Table5_2_2.html", self_contained = T) }

createhtmlTable7_2 <- function(){
kbl(Table7_2) %>%
  kable_styling(bootstrap_options = c("striped", "condensed", "hover", full_width = F, position = "left")) %>%
  column_spec(1, width = "10em") %>%
  column_spec(2, width = "20em") %>%
  column_spec(3, width = "20em") %>%
  save_kable(file = "Table7_2.html", self_contained = T) }

createhtmlTable8 <- function() {
kbl(Table8) %>%
  kable_styling(bootstrap_options = c("striped", "condensed", "hover", full_width = F, position = "left")) %>%
  column_spec(1, width = "10em") %>%
  column_spec(2, width = "20em") %>%
  column_spec(3, width = "20em") %>%
  save_kable(file = "Table8.html", self_contained = T) }

createhtmlTable4_2()
createhtmlTable4_3()
createhtmlTable5_2_2()
createhtmlTable7_2()
createhtmlTable8()


