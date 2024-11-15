import org.scalacheck.Prop.forAll
import org.scalacheck.Properties

class ExampleSpec extends Properties("String") {
  property("s1+s2 endsWith s2") = forAll { (s1: String, s2: String) =>
    (s1 + s2).endsWith(s2)
  }
}
