object Problem2:
  @main def runProblem2() =
    println("All Combos")
    allCombinablePromotions(Data.promotions).foreach(println)

    List("P1", "P3").foreach: p =>
      println(s"$p COMBOS")
      combinablePromotions(p, Data.promotions).foreach(println)

  case class Promotion(code: String, notCombinableWith: Seq[String])

  case class PromotionCombo(promotionCodes: Seq[String])

  def allCombinablePromotions(allPromotions: Seq[Promotion]): Seq[PromotionCombo] =
    2.to(allPromotions.length).toList
      .flatMap(allPromotions.map(_.code).combinations)
      .filterNot(containsCombination(allPromotions))
      .distinctByPeerSuperset
      .sorted
      .map(PromotionCombo(_))
 
  def combinablePromotions(
    promotionCode: String,
    allPromotions: Seq[Promotion]
  ): Seq[PromotionCombo] =
    allCombinablePromotions(allPromotions).filter(_.promotionCodes.contains(promotionCode))

  private def containsCombination(promotions: Seq[Promotion])(combo: Seq[String]): Boolean =
    promotions
      .flatMap(p => p.notCombinableWith.map((_, p.code)))
      .exists(combo.contains(_) && combo.contains(_))
