### Create Zone file
z2 <- zones("zone_1" = zn1, "zone_2" = zn2,  "zone_3" = zn3, "zone_4" = zn4)

p1 <- problem(pu1, zones("zone_1" = zn1, "zone_2" = zn2, 
                         "zone_3" = zn3,"zone_4" = zn4,
                         feature_names = names(zn1))) %>%
  add_max_utility_objective(c(count_tar(20), count_tar(5), count_tar(10), count_tar(65))) %>%
  add_gurobi_solver(gap = 0)

s1 <- solve(p1, force=TRUE)
setMinMax(s1)
plot(category_layer(s1), main="global")


p2 <- problem(pu1, zones("zone_1" = zn1, "zone_2" = zn2, 
                         "zone_3" = zn3,"zone_4" = zn4,
                         feature_names = names(zn1))) %>%
  add_max_utility_objective(c(count_tar(20), count_tar(5), count_tar(10), count_tar(65))) %>%
  add_rsymphony_solver()

s2 <- solve(p2, force=TRUE)
setMinMax(s2)
plot(category_layer(s2), main="global")


o2 <- compile(p2)

sol <- a$portfolio$run(opt, a$solver)