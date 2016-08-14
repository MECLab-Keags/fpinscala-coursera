
case class Book(title:String, authors:List[String])

val books: List[Book] = List(
    Book(title = "Structure and Interpretation of Computer Programs",
         authors = List("Abelson, Harald", "Sussman, Gerald J.")),
    Book(title = "Introduction to Functional Programming",
        authors = List("Bird, Rickard", "Wadler, Phil")),
    Book(title = "Effective Java",
        authors = List("Bloch, Joshua")),
    Book(title = "Java Puzzlers",
        authors = List("Bloch, Joshua", "Gafter, Neal")),
    Book(title = "Java Puzzlers 2",
        authors = List("Bloch, Joshua", "Gafter, Neal")),
    Book(title = "Programming in Scala",
        authors = List("Odersky, Martin", "Spoon, Lex", "Venners, Nill"))
)

for(b <- books; a <- b.authors; if a startsWith("Bird"))
    yield b.title

for {
    b1 <- books
    b2 <- books
    if b1.title < b2.title
    a1 <- b1.authors
    a2 <- b2.authors
    if a1 == a2
} yield a1

{ for {
    b1 <- books
    b2 <- books
    if b1.title < b2.title
    a1 <- b1.authors
    a2 <- b2.authors
    if a1 == a2
} yield a1 }.distinct

val books2: Set[Book] = Set(
    Book(title = "Structure and Interpretation of Computer Programs",
        authors = List("Abelson, Harald", "Sussman, Gerald J.")),
    Book(title = "Introduction to Functional Programming",
        authors = List("Bird, Rickard", "Wadler, Phil")),
    Book(title = "Effective Java",
        authors = List("Bloch, Joshua")),
    Book(title = "Java Puzzlers",
        authors = List("Bloch, Joshua", "Gafter, Neal")),
    Book(title = "Java Puzzlers 2",
        authors = List("Bloch, Joshua", "Gafter, Neal")),
    Book(title = "Programming in Scala",
        authors = List("Odersky, Martin", "Spoon, Lex", "Venners, Nill"))
)

for {
    b1 <- books2
    b2 <- books2
    if b1.title < b2.title
    a1 <- b1.authors
    a2 <- b2.authors
    if a1 == a2
} yield a1