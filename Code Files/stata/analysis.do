cd "/Users/markbarna/Google Drive/Education/GWU Data Science/Applied Econometrics/Final Paper/Code Files/stata"

* import csv data
import delimited "/Users/markbarna/Google Drive/Education/GWU Data Science/Applied Econometrics/Final Paper/Data/cleaned_dataset_melted.csv"

* convert fuel, emissions type to factor data
encode fuel_type, gen(fuel_type_factors)
encode emissions_type, gen(emissions_type_factors)
encode fuel_emissions_type, gen(fuel_emissions_type_factors)

* convert time column to stata time
gen statadate = clock(datetime_utc, "YMDhms")

* open already-converted dataset
use "cleaned_dataset_melted.dta"

* regress emissions and change in generation with dummies for power plant and emissions type, and their interactions
regress emissions_tons dgen_mw i.fuel_type_factors##i.emissions_type_factors, robust

* control for load
regress emissions_tons dgen_mw load_mw i.emissions_type_factors##i.fuel_type_factors, robust

* control for temperature
regress emissions_tons dgen_mw load_mw temp_f  i.emissions_type_factors##i.fuel_type_factors, robust

* control for square of temperature
regress emissions_tons dgen_mw load_mw temp_f temp_f_2 i.emissions_type_factors##i.fuel_type_factors, robust

* declare panel setting
xtset fuel_emissions_type_factors statadate

* panel regression
xtreg emissions_tons dgen_mw load_mw temp_f temp_f_2 ,fe robust cluster(fuel_emissions_type_factors)

* create log of generation varable
gen log_gen_mw = log(gen_mw)

* linear-log regression - base spec
regress emissions_tons log_gen_mw i.fuel_type_factors##i.emissions_type_factors, robust 

* control for load
regress emissions_tons log_gen_mw load_mw i.emissions_type_factors##i.fuel_type_factors, robust

* control for temperature
regress emissions_tons log_gen_mw load_mw temp_f  i.emissions_type_factors##i.fuel_type_factors, robust

* control for square of temperature
regress emissions_tons log_gen_mw load_mw temp_f temp_f_2 i.emissions_type_factors##i.fuel_type_factors, robust


* linear-log panel regression
xtreg emissions_tons log_gen_mw load_mw temp_f temp_f_2 ,fe robust
