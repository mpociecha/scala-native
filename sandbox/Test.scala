object Test {
  def main(args: Array[String]): Unit = {
    val sum = "big " + 5
    println(sum)
    assert("big 5" == sum)
  }
}
