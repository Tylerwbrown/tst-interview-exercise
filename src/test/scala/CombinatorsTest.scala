class CombinatorsTest extends munit.FunSuite:
  test("distinctByPeerSuperset"):
    val xss = List(
      List(1),
      List(1,2),
      List(1,2,3),
      List(4,5,6),
      List(1,3),
      List(1,5,6)
    )
    val expected = List(
      List(1,2,3),
      List(4,5,6),
      List(1,5,6)
    )
    assertEquals(xss.distinctByPeerSuperset, expected)
