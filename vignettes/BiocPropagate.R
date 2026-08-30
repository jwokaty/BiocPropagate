## ----default_critera----------------------------------------------------------
library(BiocPropagate)

criteria <- default_critera()
names(criteria$gates)
names(criteria$platform)


## ----bioc-checks, eval = FALSE------------------------------------------------
# criteria <- default_criteria()
# 
# # Add
# criteria <- register_criterion(
#     criteria, "always_pass",
#     function(pkg_data, branch, bioc_pkg_data, source_path)
#         list(pass = TRUE, message = NA_character_),
#         type = "gates"
#     )
# names(criteria$gates)
# 
# # Remove
# criteria <- unregister_gates(criteria, "always_pass")
# names(criteria$gates)

