import reductions.LineOfSight._

var xs = List(1,2,3,4,5,6,7,8,9,10,11,12,13,14,15,16,17,18,19,20).toArray
var (from, until) = (0, 5)
xs.drop(from).take(until).toList

var (f1,u1) = (5,10)
xs.drop(f1).take(u1 - f1).toList