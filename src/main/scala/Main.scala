object Problem1:
  @main def runProblem1() =
    val rates = List("M1", "M2", "S1", "S2")
      .map(rc => Rate(rc, if rc.startsWith("M") then "Military" else "Senior"))
    val cabinPrices = List(
      CabinPrice("CA", "M1", 200.00),
      CabinPrice("CA", "M2", 250.00),
      CabinPrice("CA", "S1", 225.00),
      CabinPrice("CA", "S2", 260.00),
      CabinPrice("CB", "M1", 230.00),
      CabinPrice("CB", "M2", 260.00),
      CabinPrice("CB", "S1", 245.00),
      CabinPrice("CB", "S2", 270.00) 
    )
    getBestGroupPrices(rates, cabinPrices).foreach(println)

  case class Rate(rateCode: String, rateGroup: String)

  case class CabinPrice(cabinCode: String, rateCode: String, price: BigDecimal)

  case class BestGroupPrice(
    cabinCode: String,
    rateCode: String,
    price: BigDecimal,
    rateGroup: String
  )

  def getBestGroupPrices(rates: Seq[Rate], prices: Seq[CabinPrice]): Seq[BestGroupPrice] = 
    val groupPrices = for
      rate  <- rates
      price <- prices
      if rate.rateCode == price.rateCode
    yield BestGroupPrice(
      cabinCode = price.cabinCode, 
      rateCode  = price.rateCode, 
      price     = price.price, 
      rateGroup = rate.rateGroup
    )

    groupPrices
      .groupBy(gp => (gp.cabinCode, gp.rateGroup))
      .values
      .toList
      .map(_.minBy(_.price))
      .sortBy(_.price)

object Problem2:
  @main def runProblem2() =
    val promotions = List(
      Promotion("P1", Seq("P3")),
      Promotion("P2", Seq("P4", "P5")),
      Promotion("P3", Seq("P1")),
      Promotion("P4", Seq("P2")),
      Promotion("P5", Seq("P2"))
    )
    println("All Combos")
    allCombinablePromotions(promotions).foreach(println)

    List("P1", "P3").foreach: p =>
      println(s"$p COMBOS")
      combinablePromotions(p, promotions).foreach(println)

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
     
extension [A](xss: Seq[Seq[A]]) 
  def distinctByPeerSuperset: Seq[Seq[A]] =
    xss.filterNot: xs =>
      xss.filter(ys => xs.forall(ys.contains)).length > 1
