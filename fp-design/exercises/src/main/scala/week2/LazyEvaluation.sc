

def expr = {
    val x = { printf("x"); 1 }
    lazy val y = { print("y"); 2 }
    def z = { print("z"); 3 }
    z + y + x + z + y + x

    // result: xzyz
    // 1) x: printed first as a result of val x = ...
    // 2) z: function call
    // 3) y: this is the first call to the lazy eval of y, so
    //       the right hand side is evaluated at this point,
    //       printing 'y' and returning 2.
    // 4) z: function call so will always evaluate.
    // 5) none: y has evaluated already at step 3 therefore y is now a value of 2.
    // 6) none: x has already been set, so value of 1 is returned.
}

expr