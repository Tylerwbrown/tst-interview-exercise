class Problem1Test extends munit.FunSuite:
  import Problem1.*

  test("getBestGroupPrices"):
    val result = getBestGroupPrices(Data.rates, Data.cabinPrices)
    val expected = List(
      BestGroupPrice("CA", "M1", 200.00, "Military"),
      BestGroupPrice("CA", "S1", 225.00, "Senior"),
      BestGroupPrice("CB", "M1", 230.00, "Military"),
      BestGroupPrice("CB", "S1", 245.00, "Senior")
    )
    assertEquals(result, expected)
