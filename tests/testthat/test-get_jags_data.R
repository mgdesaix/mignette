test_that("get_jags_data works", {
  expect_type(get_jags_data(abundance = mignette::amre_abundance,
                             nb2br_assign = mignette::amre_assign,
                             bnode_names = mignette::amre_assign$Breeding,
                             wnode_names = colnames(amre_assign)[-1],
                             model = 1), "list")
  expect_error(get_jags_data(abundance = mignette::amre_abundance,
                            nb2br_assign = mignette::amre_assign,
                            bnode_names = mignette::amre_assign$Breeding,
                            wnode_names = colnames(amre_assign)[-1],
                            model = 2))
  expect_error(get_jags_data(abundance = mignette::amre_abundance,
                             nb2br_assign = mignette::amre_assign,
                             bnode_names = mignette::amre_assign$Breeding,
                             wnode_names = colnames(amre_assign)[-1],
                             model = 3))
  expect_error(get_jags_data(abundance = mignette::amre_abundance,
                             nb2br_assign = mignette::amre_assign,
                             bnode_names = mignette::amre_assign$Breeding,
                             wnode_names = colnames(amre_assign)[-1],
                             model = 4))
  expect_error(get_jags_data(abundance = mignette::amre_abundance,
                             nb2br_assign = mignette::amre_assign,
                             bnode_names = mignette::amre_assign$Breeding,
                             wnode_names = colnames(amre_assign)[-1]))
})
