import collection.mutable.Stack
import org.scalatest.propspec.AnyPropSpec
import org.scalacheck.Prop.forAll
import org.scalatestplus.scalacheck.Checkers.check

// https://www.scalatest.org/user_guide/property_based_testing
// https://www.scalatest.org/user_guide/writing_scalacheck_style_properties

class ExampleSpec extends AnyPropSpec {

  check {
    forAll((s1: String, s2: String) => (s1 + s2).endsWith(s2))
  }
}