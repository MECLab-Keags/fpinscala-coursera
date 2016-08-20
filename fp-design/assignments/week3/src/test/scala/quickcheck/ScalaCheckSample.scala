package quickcheck

import org.scalacheck.Properties
import org.scalacheck.Prop._

object StringSpecification extends Properties("String") {
    property("startsWith") = forAll { (x:String, y:String) =>
        (x + y).startsWith(x)
    }

    property("length") = forAll { (x:String, y:String) =>
        x.length <= (x + y).length
    }
}
