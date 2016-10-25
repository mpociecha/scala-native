package java.lang

object StringSuite extends tests.Suite {
  test("concat") {
    assert("big 5" == "big" + 5)
    assert("5 big" == 5 + " big")
    assert("foo" == "foo" + "")
    assert("foo" == "" + "foo")
    assert("foobar" == "foo" + "bar")
    assert("foobarbaz" == "foo" + "bar" + "baz")
  }
}
