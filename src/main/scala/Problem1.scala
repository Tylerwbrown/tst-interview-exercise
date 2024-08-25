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
