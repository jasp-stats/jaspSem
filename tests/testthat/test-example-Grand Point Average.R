context("Example: Grand Point Average")

# This test file was auto-generated from a JASP example file.
# The JASP file is stored in the module's examples/ folder.

test_that("LatentGrowthCurve results match", {

  # Load from JASP example file
  jaspFile <- testthat::test_path("..", "..", "examples", "Grand Point Average.jasp")
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
    list(843.984199552664, 27, "Baseline model", "", 47.968827662028, 24,
     "Growth curve model", 0.00254665518276653))

  table <- results[["results"]][["modelContainer"]][["collection"]][["modelContainer_partabs"]][["collection"]][["modelContainer_partabs_latcov"]][["data"]]
  jaspTools::expect_equal_tables(table,
    list(-0.00085846086589904, 0.00488916013891318, 0.00201534963650707,
     "Intercept", 0.169290810274155, "Linear slope", 0.001466256790979,
     "<unicode><unicode>", 1.37448613974463))

  table <- results[["results"]][["modelContainer"]][["collection"]][["modelContainer_partabs"]][["collection"]][["modelContainer_partabs_latcur"]][["data"]]
  jaspTools::expect_equal_tables(table,
    list(2.06791796287375, 2.43402513169406, "Intercept", 2.2509715472839,
     "Mean", 0, 0.0933964021043546, 24.1012661790636, 0.0167068907506729,
     0.0423386997632628, "Intercept", 0.0295227952569678, "Variance",
     6.33212751299794e-06, 0.006538846941773, 4.5149849078686, 0.0472054651287348,
     0.154642052019514, "Linear slope", 0.100923758574124, "Mean",
     0.000231138171330691, 0.0274077961988652, 3.68230111760329,
     0.00199685794791987, 0.00420604760369285, "Linear slope", 0.00310145277580636,
     "Variance", 3.7309404676833e-08, 0.000563579145637059, 5.50313616076148
    ))

  table <- results[["results"]][["modelContainer"]][["collection"]][["modelContainer_partabs"]][["collection"]][["modelContainer_partabs_latreg"]][["data"]]
  jaspTools::expect_equal_tables(table,
    list(0.0412913528871243, 0.157428788054409, "Intercept", 0.0993600704707668,
     "jaspColumn2", 0.000797531488268666, 0.0296274411375317, 3.35365008437731,
     0.0265466529288868, 0.164838973463074, "Intercept", 0.0956928131959803,
     "jaspColumn11", 0.0066790872671858, 0.0352793014629399, 2.71243503209675,
     -0.0196581655418896, 0.0144231382600205, "Linear slope", -0.00261751364093458,
     "jaspColumn2", 0.763369914365672, 0.00869436991463595, -0.301058462733257,
     0.00514699113734775, 0.045729793597277, "Linear slope", 0.0254383923673124,
     "jaspColumn11", 0.0140057334222599, 0.0103529459673854, 2.4571163075177
    ))

  table <- results[["results"]][["modelContainer"]][["collection"]][["modelContainer_partabs"]][["collection"]][["modelContainer_partabs_resvar"]][["data"]]
  jaspTools::expect_equal_tables(table,
    list(0.0612975408891403, 0.0996968020983024, 0.0804971714937214, 2.22044604925031e-16,
     0.00979590990243968, "jaspColumn3", 8.21742669087569, 0.054241632081484,
     0.0852116376978466, 0.0697266348896653, 0, 0.00790065681325016,
     "jaspColumn4", 8.82542256141629, 0.0424429900809427, 0.0658298201749785,
     0.0541364051279606, 0, 0.00596613771439376, "jaspColumn5", 9.07394494051861,
     0.0223486549525005, 0.0356326322549498, 0.0289906436037252,
     0, 0.00338883199059564, "jaspColumn6", 8.55475977687215, 0.00984065247969689,
     0.0193991958011808, 0.0146199241404388, 2.0275543466397e-09,
     0.00243844871560919, "jaspColumn7", 5.99558401489138, 0.00916885736604971,
     0.0224826123461289, 0.0158257348560893, 3.16943774958389e-06,
     0.00339642847651703, "jaspColumn8", 4.65952248531326))

  plotName <- results[["results"]][["modelContainer"]][["collection"]][["modelContainer_pathplot"]][["data"]]
  testPlot <- results[["state"]][["figures"]][[plotName]][["obj"]]
  jaspTools::expect_equal_plots(testPlot, "analysis-1_figure-2_model-plot")

})

