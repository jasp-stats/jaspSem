context("Library: Grand Point Average")

# This test file was auto-generated from a JASP example file.
# The JASP file is stored in tests/testthat/jaspfiles/library/.

test_that("LatentGrowthCurve results match", {

  # Load from JASP example file
  jaspFile <- testthat::test_path("jaspfiles", "library", "Grand Point Average.jasp")
  opts <- jaspTools::analysisOptions(jaspFile)
  dataset <- jaspTools::extractDatasetFromJASPFile(jaspFile)

  # Encode and run analysis
  encoded <- jaspTools:::encodeOptionsAndDataset(opts, dataset)
  set.seed(1)
  results <- jaspTools::runAnalysis("LatentGrowthCurve", encoded$dataset, encoded$options, encodedDataset = TRUE)

  plotName <- results[["results"]][["modelContainer"]][["collection"]][["modelContainer_curveplot"]][["data"]]
  testPlot <- results[["state"]][["figures"]][[plotName]][["obj"]]
  jaspTools::expect_equal_plots(testPlot, "analysis-1_figure-1_curve-plot")

  table <- results[["results"]][["modelContainer"]][["collection"]][["modelContainer_maintab"]][["collection"]][["modelContainer_maintab_chisqtab"]][["data"]]
  jaspTools::expect_equal_tables(table,
    list(843.984199552664, 27, "Baseline model", "", 47.9688276620294,
     24, "Growth curve model", 0.00254665518276542))

  table <- results[["results"]][["modelContainer"]][["collection"]][["modelContainer_partabs"]][["collection"]][["modelContainer_partabs_latcov"]][["data"]]
  jaspTools::expect_equal_tables(table,
    list(-0.000858460865898382, 0.00488916013891401, 0.00201534963650781,
     "Intercept", 0.169290810274011, "Linear slope", 0.00146625679097904,
     "<unicode><unicode>", 1.37448613974509))

  table <- results[["results"]][["modelContainer"]][["collection"]][["modelContainer_partabs"]][["collection"]][["modelContainer_partabs_latcur"]][["data"]]
  jaspTools::expect_equal_tables(table,
    list(2.06791796287352, 2.43402513169384, "Intercept", 2.25097154728368,
     "Mean", 0, 0.0933964021043587, 24.1012661790601, 0.0167068907506701,
     0.042338699763262, "Intercept", 0.029522795256966, "Variance",
     6.33212751321999e-06, 0.00653884694177348, 4.51498490786799,
     0.0472054651287096, 0.154642052019485, "Linear slope", 0.100923758574097,
     "Mean", 0.00023113817133158, 0.0274077961988641, 3.68230111760243,
     0.00199685794791954, 0.00420604760369249, "Linear slope", 0.00310145277580602,
     "Variance", 3.7309404676833e-08, 0.000563579145637051, 5.50313616076095
    ))

  table <- results[["results"]][["modelContainer"]][["collection"]][["modelContainer_partabs"]][["collection"]][["modelContainer_partabs_latreg"]][["data"]]
  jaspTools::expect_equal_tables(table,
    list(0.0412913528872017, 0.157428788054492, "Intercept", 0.0993600704708467,
     "jaspColumn2", 0.000797531488261338, 0.029627441137533, 3.35365008437986,
     0.0265466529289202, 0.164838973463113, "Intercept", 0.0956928131960164,
     "jaspColumn11", 0.00667908726716737, 0.0352793014629413, 2.71243503209766,
     -0.0196581655418852, 0.0144231382600235, "Linear slope", -0.00261751364093085,
     "jaspColumn2", 0.76336991436599, 0.00869436991463562, -0.30105846273284,
     0.00514699113735006, 0.0457297935972777, "Linear slope", 0.0254383923673139,
     "jaspColumn11", 0.0140057334222503, 0.010352945967385, 2.45711630751794
    ))

  table <- results[["results"]][["modelContainer"]][["collection"]][["modelContainer_partabs"]][["collection"]][["modelContainer_partabs_resvar"]][["data"]]
  jaspTools::expect_equal_tables(table,
    list(0.0612975408891867, 0.0996968020983697, 0.0804971714937782, 2.22044604925031e-16,
     0.00979590990244503, "jaspColumn3", 8.21742669087701, 0.054241632081475,
     0.0852116376978342, 0.0697266348896546, 0, 0.0079006568132493,
     "jaspColumn4", 8.82542256141589, 0.0424429900809572, 0.0658298201750002,
     0.0541364051279787, 0, 0.0059661377143956, "jaspColumn5", 9.07394494051887,
     0.0223486549524998, 0.0356326322549487, 0.0289906436037243,
     0, 0.00338883199059553, "jaspColumn6", 8.55475977687215, 0.00984065247970464,
     0.0193991958011906, 0.0146199241404476, 2.0275543466397e-09,
     0.0024384487156097, "jaspColumn7", 5.9955840148937, 0.00916885736603604,
     0.0224826123461117, 0.0158257348560739, 3.16943774958389e-06,
     0.00339642847651611, "jaspColumn8", 4.65952248530996))

  plotName <- results[["results"]][["modelContainer"]][["collection"]][["modelContainer_pathplot"]][["data"]]
  testPlot <- results[["state"]][["figures"]][[plotName]][["obj"]]
  jaspTools::expect_equal_plots(testPlot, "analysis-1_figure-2_model-plot")

})

