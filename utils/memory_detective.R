
ls()
ls(parent.frame())

parent.frame()
parent.env()

## View all the 

obs_mem_df <- data.frame(var=ls(), size=sapply(ls(), function(x) format(object.size(get(x)), units="Mb")), row.names=NULL)
obs_mem_df |> View()

ls(.GlobalEnv)
ls(globalenv())


object.size(stn_data) |>  format(units="Mb")   # 152.9

#########################
## Get the total amount of memory used
# library(pryr)
# mem_used()

library(lobstr)
mem_used()
obj_size(stn_data)


##################
library(ggplot2)  # replace with your package

objs <- ls("package:ggplot2")  # list objects in the package
obj_sizes <- sapply(objs, function(x) object.size(get(x, envir = asNamespace("ggplot2"))))
total_size <- sum(obj_sizes)
print(format(total_size, units = "auto"))
print(format(total_size, units = "MB"))
