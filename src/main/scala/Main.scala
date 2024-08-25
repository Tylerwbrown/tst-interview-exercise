@main def runProblem1() =
  Problem1.getBestGroupPrices(Data.rates, Data.cabinPrices).foreach(println)

@main def runProblem2() =
  import Problem2.*
  println("All Combos")
  allCombinablePromotions(Data.promotions).foreach(println)

  List("P1", "P3").foreach: p =>
    println(s"$p COMBOS")
    combinablePromotions(p, Data.promotions).foreach(println)
