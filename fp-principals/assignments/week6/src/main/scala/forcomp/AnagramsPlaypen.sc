import forcomp.Anagrams._

dictionary

val t = "Robert".map(c => c.toLower)
                .groupBy(c => c)
                .map{ case (c,acc) => (c, acc.length)}
                .toList
                .sorted

val t1 = List("abcd", "dcba").map(w => w.map(_.toLower)
                                        .groupBy(c => c)
                                        .map{ case (c, acc) => (c, acc.length)}
                                        .toList
                                        .sorted
                             )
val t1_1 = (t1.flatten groupBy(_._1) map (t => (t._1, t._2 map(x => x._2) sum))).toList.sorted

//val find = filter (w => w == "eat" || w == "ate" || w == "tea" || w )
lazy val t2 = dictionary groupBy(w => wordOccurrences(w)) //map (t => t)
t2(List(('a', 1), ('e', 1), ('t', 1)))

val occurrences = List(('a', 2), ('b', 2))
occurrences flatMap(pair => for {
    cs <- occurrences
    o <- 1 to cs._2
    if(pair._1 != cs._1)
} yield (cs._1,o))

 for {
     pair <- occurrences
     cs <- occurrences
     o <- 1 to cs._2
     if(pair._1 != cs._1)
 } yield (cs._1,o)

1 to occurrences.length
for(pair <- occurrences take 1) yield pair

combinations(occurrences)

val sentence = List("Linux", "rulz")
sentenceAnagrams(sentence) map(_ mkString "\n")
