# password for git via terminal   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

#install.packages ("swirl")
library (swirl)
dir.create(file.path("~", "swirl", "courses"), recursive = TRUE)
dir.create(file.path("~", "swirl", "data"), recursive = TRUE)
swirl_options(swirl_courses_dir = file.path("~", "swirl", "courses"), swirl_data_dir = file.path("~", "swirl", "data"))
install_course("R Programming")
1
x <- 5+7
x
y <- x-3
y
z <- c(1.1, 9, 3.14)
?c
z
c(z, 555, 5)
c(z, 555, z)
z * 2 + 100
my_sqrt <- sqrt(z - 1)
2
my_sqrt
my_div <- z/my_sqrt
2
my_div
c(1, 2, 3, 4) + c(0, 10)
c(1, 2, 3, 4) + c(0, 10, 100)
my_div
2
