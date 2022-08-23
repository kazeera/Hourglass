# Install devtools
# install.packages("devtools")
#
# # Install Hourglass package from Github and load
# devtools::install_github("kazeera/Hourglass")
library(Hourglass)


run_from_excel("Example_IHC_sample_UserOptions - Copy.xlsx")

# Print time difference to run log
end_time <- Sys.time()
print(end_time - start_time)
# print(end_time - start_time)
x <- sprintf("Run completed on %s", format(Sys.time(), "%a %b %d %X %Y"))
print(x)
# "Run completed on Mon Aug 01 10:39:06 PM 2022"


# scales::show_col(c("#ffd966ff", "#548135ff", "#b4a7d6ff", "#9900ffff", "#351c75ff", "#2F9E97ff", "#FFFC4Dff", "#C0632Fff", "#940e26ff", "#fae7dcff", "#4b8dbeff"))
