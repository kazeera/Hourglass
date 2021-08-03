# constant variables

# Custom column levels - quantiles
LEVELS = list(l="low", i="intermed", h="high")

# Color palette for scaffold column
global_palette <- list(deserted="#CC3399", intermediary="#513573", reactive="#002060",
                       basal = "darkgoldenrod1", classical = "royalblue4", # unknown = "gray50",
                       tumour="darkslategrey", stroma="chocolate3",
                       NA_ = "black"
)
# scales::show_col(unlist(global_palette))

# Add high, int, low to colors
global_palette = c(global_palette, get_element_colors(LEVELS, get_col_palette("RdBu", rev=T)))
global_palette[[LEVELS$i]] = "azure4"

# Define boxplot axis orders:
PANC_TISS_ORDER = c("reactive", "intermediary", "deserted")

