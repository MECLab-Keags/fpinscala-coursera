import funsets.FunSets._

contains(z => z == 100, 1)
contains(z => z == 1, 1)

val u1 = union(singletonSet(1), singletonSet(2))
contains(u1, 1)
contains(u1, 2)
contains(u1, 3)

val s1 = singletonSet(1)
val s2 = singletonSet(2)
val s3 = singletonSet(3)
val s3a = singletonSet(3)
val u2 = union(union(union(s1, s2), s3), s3a)
forall(u2, x => x > 0)
exists(u2, x => x == 2)
exists2(u2, x => x == 2)

//printSet(map(s1, x => x + 1))



